(require 'circe)

(setq circe-network-options
      '(("Freenode"
	 :tls t
	 :nick "epc"
	 :sasl-username "epc"
	 )))

(provide 'setup-circe)
