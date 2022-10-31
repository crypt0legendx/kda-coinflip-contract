;; coinflip.pact

;; Define a keyset with name `coinflip-admin-keyset`.
;; Keysets cannot be created in code, thus we read them in from the load message data.
(define-keyset 'coinflip-admin-keyset (read-keyset 'coinflip-admin-keyset))

;; Define `coinflip` module
(module coinflip GOVERNANCE
  "Coinflip module"

  (defcap GOVERNANCE ()
    "Module governance capability that only allows the admin to update this module"
    ;; Check if the tx was signed with the provided keyset, fail if not
    (enforce-keyset 'coinflip-admin-keyset))

    ;; Import `coin` module while only making the `details` function available
    ;; in the `coinflip` module body
    (use coin [ details ])

    (defcap ACCOUNT-OWNER (account:string)
        "Make sure the requester owns the KDA account"

        ;; Get the guard of the given KDA account using coin.details function
        ;; and execute it using `enforce-guard`
        (enforce-guard (at 'guard (coin.details account)))
    )
)