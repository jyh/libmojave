/*
 * Interface to OpenSSL.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <unistd.h>

#ifdef SSL_ENABLED
#include <sys/socket.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>

/*
 * Print an error message.
 */
static void print_errors()
{
    char buffer[1024];
    unsigned long err;
    const char *s;

    while((err = ERR_get_error()))
        fprintf(stderr, "lm_ssl: %s\n", ERR_error_string(err, buffer));
}

/*
 * Say whether SSL is enabled.
 */
value lm_ssl_enabled(value x)
{
    return Val_true;
}

/*
 * Start the SSL server.
 */
value lm_ssl_init(value x)
{
    SSL_load_error_strings();
    SSL_library_init();
    return Val_unit;
}

/*
 * Create a new context.
 */
SSL_CTX *lm_ssl_ctx_new(value v_keyfile)
{
    char *keyfile;
    SSL_CTX *context;

    /* Parameters */
    keyfile = String_val(v_keyfile);

    /* Create a new context and initialize */
    context = SSL_CTX_new(SSLv23_method());
    if(context == 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't allocate context");
    }

    /* Load our certificate */
    if(SSL_CTX_use_certificate_chain_file(context, keyfile) == 0) {
        fprintf(stderr, "keyfile: %s\n", keyfile);
        print_errors();
        failwith("lm_ssl_ctx_new: can't assign certificate");
    }

    /* Password */
    if(SSL_CTX_use_PrivateKey_file(context, keyfile, SSL_FILETYPE_PEM) == 0) {
        fprintf(stderr, "keyfile: %s\n", keyfile);
        print_errors();
        failwith("lm_ssl_ctx_new: can't set private key");
    }

    return context;
}

/*
 * Add the DH file to the context.
 */
value lm_ssl_ctx_dhfile(SSL_CTX *context, value v_dhfile)
{
    char *dhfile;
    BIO *bio;
    DH *ret;

    /* Parameters */
    dhfile = String_val(v_dhfile);

    /* Set the DH parameters */
    bio = BIO_new_file(dhfile, "r");
    if(bio == 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't open DH file");
    }

    /* Read the file */
    ret = PEM_read_bio_DHparams(bio, NULL, NULL, NULL);
    BIO_free(bio);
    if(SSL_CTX_set_tmp_dh(context, ret) < 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't set DH params");
    }

    return Val_unit;
}

/*
 * Get data for a new connection.
 */
SSL *lm_ssl_new(SSL_CTX *context)
{
    SSL *info = SSL_new(context);
    if(context == 0)
        failwith("lm_ssl_new");
    return info;
}

/*
 * Set the file descriptor.
 */
value lm_ssl_set_fd(SSL *ssl, value v_fd)
{
    int fd = Int_val(v_fd);
    int code = SSL_set_fd(ssl, fd);
    return Val_int(code);
}

/*
 * Accept a connection.
 */
value lm_ssl_accept(SSL *ssl)
{
    int code = SSL_accept(ssl);
    if(code <= 0)
        print_errors();
    return Val_int(code);
}

/*
 * Make a connection.
 */
value lm_ssl_connect(SSL *ssl)
{
    int code = SSL_connect(ssl);
    if(code <= 0)
        print_errors();
    return Val_int(code);
}

/*
 * Read some data from the connection.
 */
value lm_ssl_read(SSL *ssl, value v_string, value v_off, value v_len)
{
    char *buf = String_val(v_string);
    int off = Int_val(v_off);
    int len = Int_val(v_len);
    int amount = SSL_read(ssl, buf + off, len);
    return Val_int(amount);
}

/*
 * Write some data to the connection.
 */
value lm_ssl_write(SSL *ssl, value v_string, value v_off, value v_len)
{
    char *buf = String_val(v_string);
    int off = Int_val(v_off);
    int len = Int_val(v_len);
    int amount = SSL_write(ssl, buf + off, len);
    return Val_int(amount);
}

/*
 * Shutdown the connection.
 */
value lm_ssl_shutdown(SSL *ssl)
{
    int code;

    /* Shutdown the connection */
    code = SSL_shutdown(ssl);

    /*
     * If we called shutdown first, the return code is always 0.
     * In this case, shutdown the TCP connection
     * and try again.
     */
    if(code == 0) {
        int fd = SSL_get_wfd(ssl);
        if(fd >= 0) {
            shutdown(fd, SHUT_WR);
            code = SSL_shutdown(ssl);
        }
    }

    /* SSL struct is no longer needed */
    SSL_free(ssl);
    return Val_int(code);
}

#else /* !SSL_ENABLED */

/*
 * Pretend SSL connection.
 */
typedef struct {
    int fd;
} SSL;

/*
 * Say whether SSL is enabled.
 */
value lm_ssl_enabled(value x)
{
    return Val_false;
}

/*
 * Start the SSL server.
 */
value lm_ssl_init(value x)
{
    return Val_unit;
}

/*
 * Create a new context.
 */
value lm_ssl_ctx_new(value x)
{
    return Val_unit;
}

value lm_ssl_ctx_dhfile(value context, value v_dhfile)
{
    return Val_unit;
}

/*
 * Get data for a new connection.
 */
SSL *lm_ssl_new(value x)
{
    SSL *sslp = (SSL *) malloc(sizeof(SSL));
    if(sslp == 0)
        failwith("lm_ssl_new");
    sslp->fd = -1;
    return sslp;
}

/*
 * Set the file descriptor.
 */
value lm_ssl_set_fd(SSL *ssl, value v_fd)
{
    ssl->fd = Int_val(v_fd);
    return Val_int(0);
}

/*
 * Accept a connection.
 */
value lm_ssl_accept(SSL *ssl)
{
    return Val_int(0);
}

/*
 * Make a connection.
 */
value lm_ssl_connect(SSL *ssl)
{
    return Val_int(0);
}

/*
 * Read some data from the connection.
 */
value lm_ssl_read(SSL *ssl, value v_string, value v_off, value v_len)
{
    char *buf = String_val(v_string);
    int off = Int_val(v_off);
    int len = Int_val(v_len);
    int amount = read(ssl->fd, buf + off, len);
    return Val_int(amount);
}

/*
 * Write some data to the connection.
 */
value lm_ssl_write(SSL *ssl, value v_string, value v_off, value v_len)
{
    char *buf = String_val(v_string);
    int off = Int_val(v_off);
    int len = Int_val(v_len);
    int amount = write(ssl->fd, buf + off, len);
    return Val_int(amount);
}

/*
 * Shutdown the connection.
 */
value lm_ssl_shutdown(SSL *ssl)
{
    free(ssl);
    return Val_int(0);
}

#endif /* !SSL_ENABLED */



