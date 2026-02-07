;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Project state for techstack-filterlist
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.0.1")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-01-03")
    (project "techstack-filterlist")
    (repo "github.com/hyperpolymath/techstack-filterlist"))

  (project-context
    (name "techstack-filterlist")
    (tagline "Declarative tech stack policy enforcement for RSR compliance")
    (tech-stack ("Ada" "TOML")))

  (current-position
    (phase "active-implementation")
    (overall-completion 40)
    (components
      ((name . "Ada CLI Core")
       (status . "in-progress")
       (completion . 50)
       (description . "14 Ada files, 329-line main, basic structure present"))

      ((name . "Definition Sets")
       (status . "complete")
       (completion . 100)
       (description . "strict.toml, moderate.toml, enterprise.toml presets"))

      ((name . "Documentation")
       (status . "complete")
       (completion . 100)
       (description . "README.adoc with usage examples"))

      ((name . "CI/CD Integration")
       (status . "planned")
       (completion . 0)
       (description . "GitHub Action not yet published"))

      ((name . "Ecosystem Integration")
       (status . "in-progress")
       (completion . 30)
       (description . "ECOSYSTEM.scm populated, needs reposystem registration")))

    (working-features
      ("TOML definition set parsing"
       "Three preset definition sets (strict/moderate/enterprise)"
       "Pattern matching for file types"
       "Severity levels (fatal/block/warn/allow)")))

  (route-to-mvp
    (milestones ()))

  (blockers-and-issues
    (critical)
    (high)
    (medium)
    (low))

  (critical-next-actions
    (immediate)
    (this-week)
    (this-month))

  (session-history ()))
