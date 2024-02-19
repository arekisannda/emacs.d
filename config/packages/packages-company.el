;;; packages-company.el --- Company Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package company :ensure t)

(use-package company-auctex :ensure t :after '(company auctex))

(use-package company-math :ensure t :after company)

(use-package company-reftex :ensure t :after company)

(provide 'packages-company)

;;; packages-company.el ends here
