/*
 * Compatibility functions for the various versions of Windows.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
#ifndef _COMPAT_WIN32_H
#define _COMPAT_WIN32_H

#ifdef WIN32

#include <windows.h>
#include <tlhelp32.h>

int ExistsOpenThread(void);
HANDLE CompatOpenThread(DWORD arg1, BOOL arg2, DWORD arg3);
BOOL CompatGetLongPathName(LPCTSTR arg1, LPTSTR arg2, DWORD arg3);
HANDLE CompatCreateToolhelp32Snapshot(DWORD arg1, DWORD arg2);
BOOL CompatThread32First(HANDLE arg1, LPTHREADENTRY32 arg2);
BOOL CompatThread32Next(HANDLE arg1, LPTHREADENTRY32 arg2);
HRESULT CompatSHGetFolderPath(HWND hwndOwner, int nFolder, HANDLE hToken, DWORD dwFlags, LPTSTR pszPath);

#endif /* WIN32 */

#endif /* _COMPAT_WIN32_H */
