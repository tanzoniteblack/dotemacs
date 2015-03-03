{:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [refactor-nrepl "0.2.2"]]
        :dependencies [[acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
                       [pjstadig/humane-test-output "0.6.0"]
                       [org.clojure/tools.nrepl "0.2.7"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
