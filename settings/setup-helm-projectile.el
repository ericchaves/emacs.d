;; setup-helm-projectile.el

(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))

  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))


(provide 'setup-helm-projectile)