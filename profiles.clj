{:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [lein-ancient "0.6.7"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       [criterium "0.4.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
