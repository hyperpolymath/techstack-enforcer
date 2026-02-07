;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for techstack-filterlist
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "techstack-filterlist")
  (type "policy-enforcement-tool")
  (purpose "Declarative tech stack policy enforcement for RSR compliance - blocks forbidden languages/patterns via CI/CD")

  (position-in-ecosystem
    (category "rsr-enforcement")
    (subcategory "language-policy")
    (unique-value
      "Only tool providing declarative language whitelist/blacklist with severity levels (fatal/block/warn/allow)"
      "Bridges RSR specifications with automated CI/CD enforcement"
      "Provides preset definition sets (strict/moderate/enterprise) for immediate use"))

  (related-projects
    ((name . "rsr-template-repo")
     (relationship . "should-include-by-default")
     (description . "Template repos should ship with techstack-filterlist config"))

    ((name . "reposystem")
     (relationship . "validates-policies-for")
     (description . "Reposystem can use techstack-filterlist to audit entire ecosystem"))

    ((name . "gitbot-fleet")
     (relationship . "provides-checks-for")
     (description . "Bots can use filterlist to validate changes before applying"))

    ((name . "scaffoldia")
     (relationship . "validates-templates")
     (description . "Scaffoldia templates should pass techstack-filterlist checks")))

  (what-this-is
    "Ada CLI tool (techstack_main.adb) for enforcing language policies"
    "TOML-based definition sets with severity levels (fatal/block/warn/allow)"
    "CI/CD integration via exit codes (0=pass, 1=violations)"
    "Three presets: strict (max safety), moderate (balanced), enterprise (practical)"
    "RSR language policy enforcement engine"
    "Self-hostable and reproducible policy checker")

  (what-this-is-not
    "NOT a linter for code quality (use language-specific linters)"
    "NOT a package manager or dependency resolver"
    "NOT a build tool"
    "NOT concerned with code style/formatting (use formatters)"))
