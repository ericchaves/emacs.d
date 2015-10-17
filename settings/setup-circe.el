(require 'circe)

(load-file "~/.emacs.d/.private.el")

(setq circe-network-options
      '(("freenode"
         :host "irc.freenode.net"
         :tls t
         :nick "epc"
         :port (6667 . 6697)
         :sasl-username "epc"
         :sasl-password freenode-password
         :nickserv-password freenode-password
         )))

(provide 'setup-circe)
