(require 'json)

(defvar aws-long-term-id nil)
(defvar aws-long-term-key nil)

(defun set-aws-session-tokens (token)
  "Set AWS session tokens into the emacs process"
  (interactive "sMFA token: ")
  (message "Token: %s" token)
  ;; Long term tokens
  (if (and (not aws-long-term-id) (not aws-long-term-key))
      (setq aws-long-term-id (getenv "AWS_ACCESS_KEY_ID")
            aws-long-term-key (getenv "AWS_SECRET_ACCESS_KEY"))
    (progn (setenv "AWS_ACCESS_KEY_ID" aws-long-term-id)
           (setenv "AWS_SECRET_ACCESS_KEY" aws-long-term-key)
           (setenv "AWS_SESSION_TOKEN")))
  (let ((aws-response (shell-command-to-string (format "aws sts get-session-token --duration-seconds 129600 --serial-number %s --token-code %s"
                                                       (getenv "MFA_TOKEN")
                                                       token))))
    (message "AWS raw response: %s" aws-response)
    (let* ((json-object-type 'plist)
           (aws-json-response (plist-get (json-read-from-string aws-response) :Credentials)))
      (setenv "AWS_ACCESS_KEY_ID" (plist-get aws-json-response :AccessKeyId))
      (setenv "AWS_SECRET_ACCESS_KEY" (plist-get aws-json-response :SecretAccessKey))
      (setenv "AWS_SESSION_TOKEN" (plist-get aws-json-response :SessionToken))
      aws-json-response)))
