augeas-lense
============

Some augesa-lense for raddb

    raddb/modules/ldap
    raddb/eap.conf
    raddb/client.conf
usage:
    augtool
    augtool> print /files/etc/raddb/modules/ldap/ldap/option[1]/
    /files/etc/raddb/modules/ldap/ldap/option[1]
    /files/etc/raddb/modules/ldap/ldap/option[1]/name = "server"
    /files/etc/raddb/modules/ldap/ldap/option[1]/value = "192.168.76.44"



