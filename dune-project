(lang dune 2.7)
(name tiger)
(version 0.1.0)

(using menhir 2.0)

(generate_opam_files true)

(source (github ayazhafiz/tiger))
(license MIT)
(authors "Ayaz Hafiz")
(maintainers "ayaz.hafiz.1@gmail.com")

(package
 (name tiger)
 (synopsis "Tiger compiler")
 (description "Tiger compiler")
 (depends
   tigerc_front
   tigerc_back
   tigerc_data
   (alcotest :with-test)))

(package
 (name tigerc_front)
 (synopsis "Tiger compiler frontend")
 (description "Tiger compiler frontend")
 (depends
   tigerc_util))

(package
 (name tigerc_back)
 (synopsis "Tiger compiler backend")
 (description "Tiger compiler backend")
 (depends
   tigerc_front
   tigerc_data))

(package
 (name tigerc_data)
 (synopsis "Tiger compiler data")
 (description "Tiger compiler data"))

(package
 (name tigerc_util)
 (synopsis "Tiger compiler utils")
 (description "Tiger compiler utils"))
