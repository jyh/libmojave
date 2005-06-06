/*
 * Compatibility functions for the various versions of Win32.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Jason Hickey, Caltech
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
#include <caml/fail.h>
#include <caml/custom.h>

#ifdef WIN32
#include <windows.h>
#include <tlhelp32.h>
#include <shlobj.h>

#include "lm_compat_win32.h"

/*
 * Pointers to the functions.
 */
static HANDLE (*OpenThreadF)(DWORD, BOOL, DWORD);
static BOOL (*GetLongPathNameF)(LPCTSTR, LPTSTR, DWORD);
static HANDLE (*CreateToolhelp32SnapshotF)(DWORD, DWORD);
static BOOL (*Thread32FirstF)(HANDLE, LPTHREADENTRY32);
static BOOL (*Thread32NextF)(HANDLE, LPTHREADENTRY32);
static HRESULT (*SHGetFolderPathF)(HWND, int, HANDLE, DWORD, LPTSTR);
static BOOL (*SHGetSpecialFolderPathF)(HWND, LPTSTR, int, BOOL);

/*
 * Compatibility.
 */
int ExistsOpenThread(void)
{
    return OpenThreadF ? 1 : 0;
}

HANDLE CompatOpenThread(DWORD arg1, BOOL arg2, DWORD arg3)
{
    return OpenThreadF(arg1, arg2, arg3);
}

BOOL CompatGetLongPathName(LPCTSTR arg1, LPTSTR arg2, DWORD arg3)
{
    BOOL b = 0;

    if(GetLongPathNameF)
        b = GetLongPathNameF(arg1, arg2, arg3);
    return b;
}

HANDLE CompatCreateToolhelp32Snapshot(DWORD arg1, DWORD arg2)
{
    return CreateToolhelp32SnapshotF(arg1, arg2);
}

BOOL CompatThread32First(HANDLE arg1, LPTHREADENTRY32 arg2)
{
    return Thread32FirstF(arg1, arg2);
}

BOOL CompatThread32Next(HANDLE arg1, LPTHREADENTRY32 arg2)
{
    return Thread32NextF(arg1, arg2);
}

HRESULT CompatSHGetFolderPath(HWND hwndOwner, int nFolder, HANDLE hToken, DWORD dwFlags, LPTSTR pszPath)
{
    if(SHGetFolderPathF) {
        fprintf(stderr, "Getting path from SHGetFolderPath\n");
        fflush(stderr);
        return SHGetFolderPathF(hwndOwner, nFolder, hToken, dwFlags, pszPath);
    }
    else if(SHGetSpecialFolderPathF) {
        BOOL fCreate = nFolder & CSIDL_FLAG_CREATE ? TRUE : FALSE;
        nFolder &= ~CSIDL_FLAG_CREATE;
        fCreate = SHGetSpecialFolderPathF(hwndOwner, pszPath, nFolder, fCreate);
        fprintf(stderr, "Getting path from SHGetSpecialFolderPath\n");
        fflush(stderr);
        return fCreate ? S_OK : E_FAIL;
    }
    else
        return E_NOTIMPL;
}

/*
 * Decide whether OpenThread is available.
 */
static void init(void)
{
    HINSTANCE hinst;
    FARPROC fp;

    hinst = GetModuleHandle(TEXT("KERNEL32"));
    if(hinst != NULL) {
        *(FARPROC *)&OpenThreadF = GetProcAddress(hinst, "OpenThread");
#ifdef UNICODE
        *(FARPROC *)&GetLongPathNameF = GetProcAddress(hinst, "GetLongPathNameW");
#else
        *(FARPROC *)&GetLongPathNameF = GetProcAddress(hinst, "GetLongPathNameA");
#endif
        *(FARPROC *)&CreateToolhelp32SnapshotF = GetProcAddress(hinst, "CreateToolHelp32Snapshot");
        *(FARPROC *)&Thread32FirstF = GetProcAddress(hinst, "Thread32First");
        *(FARPROC *)&Thread32NextF = GetProcAddress(hinst, "Thread32Next");
    }

    hinst = GetModuleHandle(TEXT("SHFOLDER"));
    if(hinst != NULL)
        *(FARPROC *)&SHGetFolderPathF = GetProcAddress(hinst, "SHGetFolderPath");

    hinst = GetModuleHandle(TEXT("SHELL32"));
    if(hinst != NULL)
        *(FARPROC *)&SHGetSpecialFolderPathF = GetProcAddress(hinst, "SHGetSpecialFolderPath");
}

/*
 * ML interface.
 */
value lm_compat_init(value v_unit)
{
    init();
    return Val_unit;
}

#else /* !WIN32 */

value lm_compat_init(value v_unit)
{
    return Val_unit;
}

#endif /* !WIN32 */

