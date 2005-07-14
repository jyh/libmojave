/*
 * System info.
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
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#ifdef WIN32
#include <windows.h>
#include <shlobj.h>
#include "lm_compat_win32.h"

/*
 * File descriptor.
 */
value int_of_fd(value fd)
{
    return Val_long((long) *(HANDLE *)Data_custom_val(fd));
}

/*
 * Home directory on Win32.
 */
value home_win32(value v_unit)
{
    CAMLparam1(v_unit);
    TCHAR path[MAX_PATH];

    if(SUCCEEDED(CompatSHGetFolderPath(NULL, CSIDL_LOCAL_APPDATA | CSIDL_FLAG_CREATE, NULL, 0, path)))
        CAMLreturn(copy_string(path));

    failwith("home_win32");
    return Val_unit;
}

/*
 * File locking.
 */
#define F_ULOCK		0
#define F_LOCK		1
#define F_TLOCK		2
#define F_TEST		3
#define F_RLOCK		4
#define F_TRLOCK	5

value lockf_win32(value v_fd, value v_kind, value v_len)
{
    HANDLE fd = *(HANDLE *)Data_custom_val(v_fd);
    int kind = Int_val(v_kind);
    int len = Int_val(v_len);
    OVERLAPPED overlapped;
    int code, flags;
    DWORD pos;

    /* Get the current position in the file */
    pos = SetFilePointer(fd, 0, 0, FILE_CURRENT);

    /* HACK: we should probably compute this correctly */
    if(len == 0)
        len = 1;

    /* Unlock case */
    if(kind == F_ULOCK)
        UnlockFile(fd, pos, 0, len, 0);
    else {
        /* Some kind of locking operation */
        switch(kind) {
        case F_LOCK:
            flags = LOCKFILE_EXCLUSIVE_LOCK;
            break;
        case F_TLOCK:
            flags = LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY;
            break;
        case F_RLOCK:
            flags = 0;
            break;
        case F_TRLOCK:
            flags = LOCKFILE_FAIL_IMMEDIATELY;
            break;
        default:
            invalid_argument("lockf_win32");
            break;
        }

        /* Set the offset */
        memset(&overlapped, 0, sizeof(overlapped));
        overlapped.Offset = pos;

        /* Perform the lock */
        enter_blocking_section();
        code = LockFileEx(fd, flags, 0, len, 0, &overlapped);
        leave_blocking_section();

        /* Fail if the lock was not successful */
        if(code == 0)
            failwith("lockf");
    }
    return Val_unit;
}

/*
 * Truncate to the current position.
 */
value ftruncate_win32(value v_fd)
{
    HANDLE fd = *(HANDLE *)Data_custom_val(v_fd);
    SetEndOfFile(fd);
    return Val_unit;
}

/************************************************************************
 * Registry.
 */

/*
 * Get the value of a registry key.
 */
value caml_registry_find(value v_hkey, value v_subkey, value v_field)
{
    char buffer[8192];
    const char *subkey, *field;
    DWORD len;
    LONG code;
    HKEY hkey;

    /* Get the arguments */
    switch(Int_val(v_hkey)) {
    case 0:
        hkey = HKEY_CLASSES_ROOT;
        break;
    case 1:
        hkey = HKEY_CURRENT_CONFIG;
        break;
    case 2:
        hkey = HKEY_CURRENT_USER;
        break;
    case 3:
        hkey = HKEY_LOCAL_MACHINE;
        break;
    case 4:
        hkey = HKEY_USERS;
        break;
    default:
        caml_failwith("get_registry: unknown handle");
        break;
    }

    /* Ask Windows */
    subkey = String_val(v_subkey);
    field = String_val(v_field);
    len = sizeof(buffer);

#if 0
    code = RegGetValue(hkey, subkey, field, RRF_RT_REG_SZ, NULL, (LPVOID) buffer, &len);
    if(code != ERROR_SUCCESS)
        caml_raise_not_found();
#else
    {
        HKEY hand;

        code = RegOpenKeyEx(hkey, subkey, 0, KEY_QUERY_VALUE, &hand);
        if(code != ERROR_SUCCESS)
            caml_raise_not_found();

        code = RegQueryValueEx(hand, field, NULL, NULL, (LPBYTE) buffer, &len);
        RegCloseKey(hand);
        if(code != ERROR_SUCCESS)
            caml_raise_not_found();
    }
#endif

    /* Got the value */
    return copy_string(buffer);
}

#else /* WIN32 */

value int_of_fd(value fd)
{
    return fd;
}

value home_win32(value v_unit)
{
    caml_failwith("home_win32: not to be used except on Win32");
    return Val_unit;
}

value lockf_win32(value v_fd, value v_kind, value v_len)
{
    caml_failwith("lockf_win32: not to be used except on Win32");
    return Val_unit;
}

value ftruncate_win32(value v_fd)
{
    caml_failwith("ftruncate_current_win32: not to be used except on Win32");
    return Val_unit;
}

value caml_registry_open(value v_hand, value v_subkey)
{
    caml_raise_not_found();
    return Val_unit;
}

value caml_registry_find(value v_key, value v_field)
{
    caml_raise_not_found();
    return Val_unit;
}

value caml_registry_close(value v_key)
{
    return Val_unit;
}

#endif /* !WIN32 */
