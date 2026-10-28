(* Reproduces the C configuration OMake's configure step performs for
   libmojave (omake's lib/configure/ncurses.om and fs_case_sensitive.om). *)
module C = Configurator.V1

let getattrlist_test = {|
#include <sys/param.h>
#include <sys/mount.h>
#include <sys/attr.h>
#include <unistd.h>
typedef struct vol_caps_buf {
    unsigned long size;
    vol_capabilities_attr_t caps;
} vol_caps_buf_t;
int main(int argc, char *argv[]) {
    struct statfs stat;
    statfs(".", &stat);
    struct attrlist alist;
    alist.bitmapcount = ATTR_BIT_MAP_COUNT;
    alist.volattr = ATTR_VOL_CAPABILITIES;
    vol_caps_buf_t buffer;
    getattrlist(stat.f_mntonname, &alist, &buffer, sizeof(buffer), 0);
    int foo = ATTR_VOL_CAPABILITIES + VOL_CAPABILITIES_FORMAT +
            VOL_CAP_FMT_CASE_SENSITIVE;
    return foo == 0;
}
|}

let ncurses_test term_h = Printf.sprintf {|
#include <curses.h>
#include <%s>
int main(void) { initscr(); setupterm(0, 1, 0); endwin(); return 0; }
|} term_h

let () =
  C.main ~name:"libmojave" (fun c ->
      let win32 = C.ocaml_config_var c "os_type" = Some "Win32" in
      let cflags = ref [ "-DHAVE_SNPRINTF" ] and libs = ref [] in
      if (not win32) && C.c_test c ~link_flags:[] getattrlist_test then
        cflags := "-DDETECT_FS_CASE_SENSITIVE_GETATTRLIST" :: !cflags;
      if not win32 then begin
        let lncurses = [ "-lncurses" ] in
        if C.c_test c ~link_flags:lncurses (ncurses_test "term.h") then begin
          cflags := "-DNCURSES" :: !cflags; libs := lncurses
        end else if C.c_test c ~link_flags:lncurses (ncurses_test "ncurses/term.h")
        then begin
          cflags := "-DNCURSES" :: "-DTERMH_IN_NCURSES" :: !cflags; libs := lncurses
        end
      end;
      C.Flags.write_sexp "c_flags.sexp" (List.rev !cflags);
      C.Flags.write_sexp "c_library_flags.sexp" !libs)
