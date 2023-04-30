//===- memberkindplugin.cpp ---------------------------------------------===//
//
// clang plugin which prints the name of any class template member which has different kind in an explicit specialization.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <fstream>

using namespace clang;

namespace {

// This class maintains a list of scsanned header and cpp files. For the moment files are only added if they contain a declaration
// that the AST visitor gets. To reduce the size of the compiledfiles.txt file it is first read in and only _new_ files are added
// and then appended to the file at the end of the run. This allows reasonable uniqueness even for parallel compilations although
// in that case some overlap may result, which has to be fitered after the build(s) complete.
// Note that if builds in disparate directory trees are invcluded there may still be duplicated filenames if there are multiple
// copies on disk.
// Each line consists of the normalized filename followed by lie count and byte count. Just before closing after writing out the
// new file info a sum row is written containing:
// Sum so far: <TUs> <Files> <Lines> <Bytes>
class FileList {
public:
    FileList(const std::string& fileListName) : m_fileListName(fileListName) {
        // Load file.
        std::ifstream file(fileListName);
        std::string line;
        while (file) {
            getline(file, line);
            if (line.substr(0, 10) == "Sum so far")
                m_tus++;
            else {
                size_t pos = line.find(',');
                if (pos == std::string::npos)
                    continue;   // Bad line
                std::string filename = line.substr(0, pos);
                auto iter = m_files.find(filename);
                if (iter != m_files.end())
                    continue;   // Repeated file

                // Parse rest of line to get lines and bytes for this file
                size_t pos2 = line.find(',', pos + 1);
                if (pos2 == std::string::npos)
                    continue;
                auto lineStr = line.substr(pos + 1, pos2 - pos);
                auto lines = stoi(lineStr);

                size_t pos3 = line.find(',', pos2 + 1);
                if (pos3 == std::string::npos)
                    continue;
                auto bytesStr = line.substr(pos2 + 1, pos3 - pos2);
                auto bytes = stoi(bytesStr);

                m_lines += lines;
                m_bytes += bytes;
                m_files[filename] = false;
            }
        }

        m_lastFile = m_files.end();
    }

    ~FileList() {
        // Write the New files for this compilation and the sum line.
        std::ofstream file(m_fileListName, std::ios::app);

        for (auto& elem : m_files) {
            if (elem.second) {
                // New file. Open and count its lines and bytes.
                size_t bytes = 0;
                size_t lines = 0;
                std::ifstream ff(elem.first);
                std::string line;
                while (ff) {
                    getline(ff, line);
                    lines++;
                    bytes += line.size();
                }
                file << elem.first.c_str() << ", " << lines << ", " << bytes << std::endl;     // Flush to ensure append

                m_lines += lines;
                m_bytes += bytes;
            }
        }

        file << "Sum so far: " << m_tus << " TUs. " << m_files.size() << " Files. " << m_lines << " Lines. " << m_bytes << " Bytes." << std::endl;
    }

    void add(llvm::StringRef fileName) {
        // Add if not found (normalize path first)
        auto fn = fileName.str();
        llvm::errs() << "Adding file: " << fn << "\n";
        if (m_lastFile != m_files.end() && fn == m_lastFile->first)
            return;     // Declaration in same file as last.

        // Try finding hte file
        m_lastFile = m_files.find(fn);

        // Append m_files if not found
        if (m_lastFile == m_files.end())
            m_lastFile = m_files.emplace(std::move(fn), true).first;
    }

private:
    std::unordered_map<std::string, bool> m_files;      // Bool is false for existing files, true for new files.
    std::unordered_map<std::string, bool>::iterator m_lastFile;
    std::string m_fileListName;
    size_t m_tus = 0;
    size_t m_lines = 0;
    size_t m_bytes = 0;
};


class MemberKindConsumer : public ASTConsumer {
public:
    MemberKindConsumer(CompilerInstance& instance) : m_instance(instance) {}

    void HandleTranslationUnit(ASTContext& context) override {
        struct Visitor : public RecursiveASTVisitor<Visitor> {
        public:
            Visitor(ASTContext& context) : m_context(context), m_fileList("/home/bengtg/CompiledFiles.txt") {
                m_warningList.open("/home/bengtg/KindMismatches.txt", std::ios::app);
            }

            bool VisitNamespaceDecl(NamespaceDecl* declaration) {
                llvm::errs() << "VNamespace: " << declaration->getNameAsString() << "\n";
                m_fileList.add(m_context.getSourceManager().getFilename(declaration->getLocation()));
                return true;
            }
            bool VisitClassTemplateDecl(ClassTemplateDecl* declaration) {
                NamedDecl* named = declaration->getTemplatedDecl();
                if (named->getKind() == Decl::Kind::CXXRecord) {
                    CXXRecordDecl *rec = static_cast<CXXRecordDecl*>(named);
                    llvm::errs() << "VClass template: " << rec->getNameAsString() << "\n";
                    handleClass(rec);
                }
//                 if (named->getKind() == Decl::Kind::ClassTemplateSpecialization) {
//                     CXXRecordDecl *rec = static_cast<CXXRecordDecl *>(named);
//                     llvm::errs() << "Class template specialization: " << rec->getNameAsString() << "\n";
//                     handleClass(rec);
//                 }
//                 if (named->getKind() == Decl::Kind::ClassTemplatePartialSpecialization) {
//                     CXXRecordDecl *rec = static_cast<CXXRecordDecl *>(named);
//                     llvm::errs() << "Class template partial specialization: " << rec->getNameAsString() << "\n";
//                     handleClass(rec);
//                 }
// 
                return true;
            }

            bool VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl *declaration) {
                if (declaration->getKind() == Decl::Kind::ClassTemplatePartialSpecialization)
                    return true;

                llvm::errs() << "VClass template specialization: " << declaration->getNameAsString() << "\n";
                handleClass(declaration);
                return true;
            }
            bool VisitClassTemplatePartialSpecializationDecl(ClassTemplatePartialSpecializationDecl *declaration) {
                llvm::errs() << "VClass template partial specialization: " << declaration->getNameAsString() << "\n";
                handleClass(declaration);
                return true;
            }

            bool VisitDependentTemplateSpecializationType(DependentTemplateSpecializationType* dep) {
                auto nns = dep->getQualifier();
                llvm::errs() << "Type disambiguation: ";
                nns->dump(llvm::errs());
                llvm::errs() << "\n";
                return true;
            }

            bool VisitFunctionDecl(FunctionDecl* decl) {
                m_fileList.add(m_context.getSourceManager().getFilename(decl->getLocation()));
            }

            void handleClass(CXXRecordDecl* rec) {
                std::string name = rec->getQualifiedNameAsString();
                m_fileList.add(m_context.getSourceManager().getFilename(rec->getLocation()));
                m_classes[name].process(*rec, m_warningList, m_context.getSourceManager());
            }

            struct Data {
                void process(CXXRecordDecl& rec, std::ofstream& warningList, SourceManager& sm) {
                    llvm::errs() << "Processing " << rec.getQualifiedNameAsString() << "\n";
                    for (auto decl : rec.decls()) {
                        static const ASTNodeKind usingShadowDeclKind = ASTNodeKind::getFromNodeKind<UsingShadowDecl>();
                        ASTNodeKind kind = ASTNodeKind::getFromNode(*decl);
                        while (usingShadowDeclKind.isBaseOf(kind)) {
                            // When inheriting through using declarations the inherited names are represented by
                            // UsingShadowDecl objects we need to look through.
                            decl = static_cast<UsingShadowDecl&>(*decl).getTargetDecl();
                            kind = ASTNodeKind::getFromNode(*decl);
                        }

                        ASTNodeKind namedKind = ASTNodeKind::getFromNodeKind<NamedDecl>();
                        if (!namedKind.isBaseOf(kind))
                            return;

                        auto name = static_cast<NamedDecl&>(*decl).getNameAsString();

                        static const ASTNodeKind typeKind = ASTNodeKind::getFromNodeKind<TypeDecl>();
                        static const ASTNodeKind valueKind = ASTNodeKind::getFromNodeKind<ValueDecl>();
                        static const ASTNodeKind classTemplateKind = ASTNodeKind::getFromNodeKind<ClassTemplateDecl>();
                        static const ASTNodeKind typeAliasTemplateKind = ASTNodeKind::getFromNodeKind<TypeAliasTemplateDecl>();
                        static const ASTNodeKind templateSpecializationKind = ASTNodeKind::getFromNodeKind<ClassTemplateSpecializationDecl>();
                        static const ASTNodeKind templatePartialSpecializationKind = ASTNodeKind::getFromNodeKind<ClassTemplatePartialSpecializationDecl>();
                        static const ASTNodeKind functionTemplateKind = ASTNodeKind::getFromNodeKind<FunctionTemplateDecl>();
                        static const ASTNodeKind variableTemplateKind = ASTNodeKind::getFromNodeKind<VarTemplateDecl>();
                        static const ASTNodeKind usingDeclKind = ASTNodeKind::getFromNodeKind<UsingDecl>();

                        Cat cat;
                        if (classTemplateKind.isBaseOf(kind) || templateSpecializationKind.isBaseOf(kind) || variableTemplateKind.isBaseOf(kind) || typeAliasTemplateKind.isBaseOf(kind))
                            cat = cTemplate;
                        else if (typeKind.isBaseOf(kind)) {
                            static const ASTNodeKind recordKind = ASTNodeKind::getFromNodeKind<RecordDecl>();
                            if (recordKind.isBaseOf(kind) && static_cast<RecordDecl&>(*decl).isAnonymousStructOrUnion())
                                continue;       // Disregard the anonymous union, its implicit object appears in the same scope too.

                            cat = cType;
                        }
                        else if (valueKind.isBaseOf(kind) || functionTemplateKind.isBaseOf(kind))
                            cat = cValue;
                        else if (usingDeclKind.isBaseOf(kind))
                            continue;       // These are processed via the generated UsingShadowDecl ojects.
                        else
                            cat = cOther;

                        if (cat == cType && rec.getNameAsString() == name)
                            continue;           // Exclude the own type shortcut as it conflicts (here) with constructors

                        llvm::errs() << "  Named member: " << name << "\n";
                        auto iter = m_members.find(name);
                        if (iter == m_members.end())
                            m_members[name] = { cat, decl->getLocation() };
                        else {
                            if (iter->second.m_category != cat)
                                warningList << decl->getLocation().printToString(sm) << ": " <<
                                        rec.getQualifiedNameAsString() << "::" << name << " is a " << toString(cat) <<
                                        " but first declared as a " << toString(iter->second.m_category) <<
                                        " at " << iter->second.m_location.printToString(sm) << std::endl;
                        }
                    }
                }

                enum Cat {
                    cTemplate,
                    cType,
                    cValue,
                    cOther
                };

                const char* toString(Cat cat) {
                    switch (cat) {
                    case cTemplate:
                        return "template";

                    case cType:
                        return "type";

                    case cValue:
                        return "value";

                    default:
                        return "other";
                    }
                };

                struct Info {
                    Cat m_category;
                    SourceLocation m_location;
                };

                std::map<std::string, Info> m_members;
            };

            std::map<std::string, Data> m_classes;
            ASTContext& m_context;      // To print source locations
            std::ofstream m_warningList;
            FileList m_fileList;
        } v(context);

        v.TraverseAST(context);
    }

private:
    CompilerInstance& m_instance;
};


class MemberKindAction : public PluginASTAction {
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
        llvm::errs() << "Starting\n";
        return std::make_unique<MemberKindConsumer>(CI);
    }

    bool ParseArgs(const CompilerInstance &CI, const std::vector<std::string> &args) override {
        for (unsigned i = 0, e = args.size(); i != e; ++i) {
            llvm::errs() << "MemberKind arg = " << args[i] << "\n";

            // Example error handling.
            DiagnosticsEngine &D = CI.getDiagnostics();
            if (args[i] == "-an-error") {
                unsigned DiagID = D.getCustomDiagID(DiagnosticsEngine::Error, "invalid argument '%0'");

                D.Report(DiagID) << args[i];
                return false;
            }
            else if (args[i] == "-parse-template") {
                if (i + 1 >= e) {
                    D.Report(D.getCustomDiagID(DiagnosticsEngine::Error,
                                               "missing -parse-template argument"));
                    return false;
                }

                ++i;
            }
        }
        if (!args.empty() && args[0] == "help")
            PrintHelp(llvm::errs());

        return true;
    }

    void PrintHelp(llvm::raw_ostream& ros) {
        ros << "Help for PrintFunctionNames plugin goes here\n";
    }

    ActionType getActionType() override {
        return AddAfterMainAction;
    }

};

}

static FrontendPluginRegistry::Add<MemberKindAction>  X("check-specialization-member-kind", "Print qualified names of class members changing kind in a explicit specialization");

