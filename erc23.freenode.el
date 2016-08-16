(add-to-list 'erc-nickserv-alist
             '(freenode "NickServ!NickServ@services."
                        "/msg\\s-NickServ\\s-identify\\s-<password>"
                        "NickServ"
                        "identify"
                        nil
                        nil
                        "You are now identified for \\S-+\\."))
