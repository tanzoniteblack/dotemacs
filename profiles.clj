{:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [lein-ancient "0.6.7"]
                  [refactor-nrepl "1.1.0"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       [acyclic/squiggly-clojure "0.1.3-SNAPSHOT"]
                       [criterium "0.4.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
