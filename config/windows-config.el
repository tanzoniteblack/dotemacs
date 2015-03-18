;; Windows-NT specific configuration
(setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;"
					   "C:\\Program Files\\Java\\jdk1.8.0_40\\bin;"
					   (getenv "PATH")))
(add-to-list 'exec-path "c:/Program Files (x86)/Git/bin")
(add-to-list 'exec-path "c:/Program Files (x86)/Aspell/bin")
(add-to-list 'exec-path "c:/Program Files/Java/jdk1.8.0_40/bin")
