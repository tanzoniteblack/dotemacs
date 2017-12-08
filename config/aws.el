(require 'json)

(defvar aws-long-term-id nil)
(defvar aws-long-term-key nil)
(defvar aws-mfa-arn nil)

(defun aws-two-factor--set-vars-from-env ()
  (when (not (and aws-long-term-id
				  aws-long-term-key
				  aws-mfa-arn))
	(setq aws-long-term-id (getenv "AWS_ACCESS_KEY_ID")
		  aws-long-term-key (getenv "AWS_SECRET_ACCESS_KEY")
		  aws-mfa-arn (getenv "MFA_ARN"))))

(defun aws-two-factor--proper-aws-keys-exist? ()
  (if (or (and aws-long-term-id
			   aws-long-term-key
			   aws-mfa-arn)
		  (and (getenv "AWS_ACCESS_KEY_ID")
			   (getenv "AWS_SECRET_ACCESS_KEY")
			   (getenv "MFA_ARN")))
	  t
	(error "Please set either aws-long-term-id, aws-long-term-key, & aws-mfa-arn or make sure the following environment variables are set: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, & MFA_ARN")))

(defun set-aws-session-tokens (token)
  "Set AWS session tokens into the emacs process"
  (interactive "sMFA token: ")
  (message "Token: %s" token)
  (when (aws-two-factor--proper-aws-keys-exist?)
	;; Long term tokens
	(aws-two-factor--set-vars-from-env)
	(let ((aws-response (shell-command-to-string (format "aws sts get-session-token --duration-seconds 129600 --serial-number %s --token-code %s"
														 aws-mfa-arn
														 token))))
	  (message "AWS raw response: %s" aws-response)
	  (let* ((json-object-type 'plist)
			 (aws-json-response (plist-get (json-read-from-string aws-response) :Credentials)))
		(setenv "AWS_ACCESS_KEY_ID" (plist-get aws-json-response :AccessKeyId))
		(setenv "AWS_SECRET_ACCESS_KEY" (plist-get aws-json-response :SecretAccessKey))
		(setenv "AWS_SESSION_TOKEN" (plist-get aws-json-response :SessionToken))
		aws-json-response))))
