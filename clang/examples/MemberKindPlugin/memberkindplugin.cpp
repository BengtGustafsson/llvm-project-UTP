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

using namespace clang;

namespace {

class MemberKindConsumer : public ASTConsumer {
    CompilerInstance &Instance;

public:
    MemberKindConsumer(CompilerInstance &Instance, std::set<std::string> ParsedTemplates) : Instance(Instance) {}

    void HandleTranslationUnit(ASTContext& context) override {
      struct Visitor : public RecursiveASTVisitor<Visitor> {
        bool VisitClassTemplateDecl(ClassTemplateDecl* declaration) {
          NamedDecl* named = declaration->getTemplatedDecl();
          if (named->getKind() == Decl::Kind::CXXRecord) {
            CXXRecordDecl *rec = static_cast<CXXRecordDecl*>(named);
            llvm::errs() << "Class template: " << rec->getNameAsString() << "\n";
            handleClass(rec);
          }
          if (named->getKind() ==
              Decl::Kind::ClassTemplateSpecialization) {
            CXXRecordDecl *rec = static_cast<CXXRecordDecl *>(named);
            llvm::errs()
                << "Class template specialization: " << rec->getNameAsString() << "\n";
            handleClass(rec);
          }
          if (named->getKind() ==
              Decl::Kind::ClassTemplatePartialSpecialization) {
            CXXRecordDecl *rec = static_cast<CXXRecordDecl *>(named);
            llvm::errs()
                << "Class template partial specialization: " << rec->getNameAsString() << "\n";
            handleClass(rec);
          }

          return true;
        }

        bool VisitClassTemplateSpecializationDecl(
          ClassTemplateSpecializationDecl *declaration) {
          if (declaration->getKind() ==
              Decl::Kind::ClassTemplatePartialSpecialization)
            return true;

          llvm::errs() << "Class template specialization: "
              << declaration->getNameAsString() << "\n";
          handleClass(declaration);
          return true;
        }
        bool VisitClassTemplatePartialSpecializationDecl(
          ClassTemplatePartialSpecializationDecl *declaration) {
          llvm::errs() << "Class template partial specialization: "
              << declaration->getNameAsString() << "\n";
          handleClass(declaration);
          return true;
        }

        void handleClass(CXXRecordDecl* rec) {
          std::string name = rec->getNameAsString();
          m_classes[name].process(*rec);
        }

        struct Data {
          void process(CXXRecordDecl& rec) {
            llvm::errs() << "Processing " << rec.getNameAsString() << "\n";
            for (auto& decl : rec.decls()) {
              ASTNodeKind kind = ASTNodeKind::getFromNode(*decl);
              ASTNodeKind namedKind = ASTNodeKind::getFromNodeKind<NamedDecl>();
              if (!namedKind.isBaseOf(kind))
                return;

              auto name = static_cast<NamedDecl&>(*decl).getNameAsString();
              llvm::errs() << "  Named member: " << name << "\n";

              ASTNodeKind typeKind = ASTNodeKind::getFromNodeKind<TypeDecl>();
              ASTNodeKind valueKind = ASTNodeKind::getFromNodeKind<ValueDecl>();
              ASTNodeKind templateKind = ASTNodeKind::getFromNodeKind<TemplateDecl>();
              Cat cat;
              if (typeKind.isBaseOf(kind))
                cat = cType;
              else if (valueKind.isBaseOf(kind))
                cat = cValue;
              else if (templateKind.isBaseOf(kind))
                cat = cTemplate;
              else
                cat = cOther;

              auto iter = m_members.find(name);
              if (iter == m_members.end())
                m_members[name] = cat;
              else {
                if (iter->second != cat)
                  llvm::errs() << "  Named member: " << name << " has different kind!\n";
              }
            }
          }

          enum Cat {
            cTemplate,
            cType,
            cValue,
            cOther
          };

          std::map<std::string, Cat> m_members;
        };

        std::map<std::string, Data> m_classes;
      } v;

      v.TraverseDecl(context.getTranslationUnitDecl());
    }
};


class MemberKindAction : public PluginASTAction {
    std::set<std::string> ParsedTemplates;
protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
        return std::make_unique<MemberKindConsumer>(CI, ParsedTemplates);
    }

    bool ParseArgs(const CompilerInstance &CI,
                   const std::vector<std::string> &args) override {
        for (unsigned i = 0, e = args.size(); i != e; ++i) {
            llvm::errs() << "MemberKind arg = " << args[i] << "\n";

            // Example error handling.
            DiagnosticsEngine &D = CI.getDiagnostics();
            if (args[i] == "-an-error") {
                unsigned DiagID = D.getCustomDiagID(DiagnosticsEngine::Error,
                    "invalid argument '%0'");
                D.Report(DiagID) << args[i];
                return false;
            } else if (args[i] == "-parse-template") {
                if (i + 1 >= e) {
                    D.Report(D.getCustomDiagID(DiagnosticsEngine::Error,
                                               "missing -parse-template argument"));
                    return false;
                }
                ++i;
                ParsedTemplates.insert(args[i]);
            }
        }
        if (!args.empty() && args[0] == "help")
            PrintHelp(llvm::errs());

        return true;
    }
    void PrintHelp(llvm::raw_ostream& ros) {
        ros << "Help for PrintFunctionNames plugin goes here\n";
    }

};

}

static FrontendPluginRegistry::Add<MemberKindAction>  X("check-specialization-member-kind", "Print qualified names of class members changing kind in a explicit specialization");

int includeMemberkindAction;