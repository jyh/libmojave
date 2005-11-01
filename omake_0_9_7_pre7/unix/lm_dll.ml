(*
 * Dynamic link libraries.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 *)
open Lm_string_set
open Lm_symbol
open Lm_printf

(*
 * Flags when a library is opened.
 *)
type open_flag =
   RTLD_LAZY
 | RTLD_NOW
 | RTLD_GLOBAL

type open_flags = open_flag list

(*
 * The info loaded from the DLL.
 *)

(*
 * An object is represented as an array of
 * named values.
 *
 * Runtime object do not include name or type information.
 * They are just an array where the first entry is the name
 * of the object.
 *
 * The type names use C notation, where all typedefs have
 * been unfolded.
 *
 * The values associated with each of the following types
 * are in the normal format.
 *     void         : the value is ()
 *     int          : the value is an integer
 *     char         : same as int
 *     char *       : the value is a string
 *     float        : the value is a float
 *     double       : same as float
 *     object       : the value is an array of values,
 *         where the first element is the name of
 *         the object (so you can get the type information).
 *
 * Values in any other type, like the following, are C pointers.
 * You should treat these values as abstract, and you should carry
 * aroun
 *     void *
 *     struct foo *
 *     void (*)(int, float)
 *)
 *)
type dll_object_field_type =
   { object_field_name : symbol;
     object_field_type : symbol
   }

type dll_object_type =
   { object_name   : symbol;
     object_fields : dll_object_field_type array
   }

(*
 * Enumerations associate integers with values.
 *)
type dll_enum_field =
   { enum_field_name  : symbol;
     enum_field_value : int
   }

type dll_enum =
   { enum_name   : symbol;
     enum_fields : dll_enum_field array
   }

(*
 * A function has a name, and type information for
 * the arguments and the result.
 *
 * The value itself is abstract.
 *)
type 'a dll_value =
   { value_name        : symbol;
     value_fun         : 'a;
     value_arg_types   : symbol array;
     value_result_type : symbol
   }

(*
 * All the info from the DLL.
 *)
type 'a dll =
   { dll_objects : dll_object_type array;
     dll_enums   : dll_enum array;
     dll_values  : 'a dll_value array;
     dll_globals : Obj.t
   }

(*
 * Values to pass to the C functions.
 *)
type dll_val =
   ValUnit
 | ValBool of bool
 | ValInt of int
 | ValFloat of float
 | ValString of string
 | ValObject of symbol * Obj.t array
 | ValAbstract of symbol * Obj.t

(*
 * The raw info loaded from static library.
 *)
type dll_export

(*
 * Types for C values.
 *)
type t_void = unit
type t_char = char
type t_int  = int
type t_float = float
type t_double = float
type t___builtin_va_list

type 'a dll_pointer
type ('args, 'res) dll_function
type dll_object

external p_coerce : 'a dll_pointer -> 'b dll_pointer = "%identity"

(*
 * For creating tuples from singleton values.
 *)
type 'a dll_singleton_tuple =
   SingletonTuple of 'a

(*
 * The DLL interface.
 *)
module type DllSig =
sig
   type t

   type dll_fun
   type dll_sym

   (*
    * Open the DLL.
    *)
   val opendll       : string -> open_flags -> t option

   (*
    * Open the static info.
    *)
   val open_static   : dll_export -> t

   (*
    * Get the value of an enum.
    * This is mainly for use by generated code.
    *)
   val get_enum      : t -> int -> int -> int

   (*
    * Get the global variables.
    *)
   val get_globals   : t -> Obj.t

   (*
    * Get the info of all the values in the DLL.
    *)
   val info           : t -> dll_fun dll

   (*
    * Use this function if you want to handle callbacks.
    * Eventually, it would be good to make the return value general.
    *)
   val set_callback_handler : t -> (Obj.t -> int) -> unit

   (*
    * Apply a function.  This should be reasonably safe, as long
    * as the DLL has reported its type information correctly.
    *)
   val apply : dll_fun -> dll_val array -> dll_val

   (* This is raised if an argument can't be coerced to the right type *)
   exception TypeError of symbol * dll_val

   (* This is raised if the number of arguments doesn't match *)
   exception ArityMismatch of dll_fun * int * int

   (*
    * !!!WARNING!!!
    *
    * The following function bypasses the type checking
    * and calls the DLL function directly.  This is for clients
    * that want to do the typechecking themselves.  The performance
    * is somewhat better.
    *)
   val unsafe_get     : dll_fun -> dll_sym
   val unsafe_apply   : dll_sym -> Obj.t array -> Obj.t

   (*
    * We also need NULL pointers directly.
    *)
   val null           : Obj.t
   val is_null        : 'a -> bool

   val p_NULL         : 'a dll_pointer

   val pointer_of_int : int -> Obj.t
   val int_of_pointer : Obj.t -> int
end;;

module Dll : DllSig =
struct
   (* The abstract type of DLL values *)
   type dll_sym

   (* The function to set the callback handler *)
   type set_handler

   (* Function with type information *)
   type dll_fun = dll_sym dll_value

   (* The DLL info *)
   type t = dll_fun dll * set_handler

   (*
    * The raw information returned by the DLL.
    *)
   type raw_object_field =
      { raw_object_field_name : string;
        raw_object_field_type : string
      }

   type raw_object =
      { raw_object_name : string;
        raw_object_fields : raw_object_field array
      }

   type raw_enum_field =
      { raw_enum_field_name  : string;
        raw_enum_field_value : int
      }

   type raw_enum =
      { raw_enum_name : string;
        raw_enum_fields : raw_enum_field array
      }

   type raw_value =
      { raw_value_name : string;
        raw_value_fun  : dll_sym;
        raw_value_arg_types : raw_object_field array;
        raw_value_result_type : string
      }

   type raw_info =
      Export of raw_object array * raw_enum array * raw_value array * set_handler * Obj.t
    | NoExport

   (* This is raised if an argument can't be coerced to the right type *)
   exception TypeError of symbol * dll_val

   (* This is raised if the number of arguments doesn't match *)
   exception ArityMismatch of dll_fun * int * int

   (************************************************************************
    * Open the DLL.
    *)
   external dlopen  : string -> open_flags -> raw_info = "lm_dlopen";;

   (*
    * Convert the DLL info.
    *)
   let build_object_field info =
      let { raw_object_field_name = name;
            raw_object_field_type = ty
          } = info
      in
         { object_field_name = Lm_symbol.add name;
           object_field_type = Lm_symbol.add ty
         }

   let build_object info =
      let { raw_object_name = name;
            raw_object_fields = fields
          } = info
      in
         { object_name = Lm_symbol.add name;
           object_fields = Array.map build_object_field fields
         }

   let build_enum_field info =
      let { raw_enum_field_name = name;
            raw_enum_field_value = i
          } = info
      in
         { enum_field_name = Lm_symbol.add name;
           enum_field_value = i
         }

   let build_enum info =
      let { raw_enum_name = name;
            raw_enum_fields = fields
          } = info
      in
         { enum_name = Lm_symbol.add name;
           enum_fields = Array.map build_enum_field fields
         }

   let pp_print_string_array buf sl =
      Array.iter (fun s -> fprintf buf " %s," s) sl

   let build_value info =
      let { raw_value_name = name;
            raw_value_fun = sym;
            raw_value_arg_types = arg_types;
            raw_value_result_type = result_type
          } = info
      in
      let info =
         { value_name = Lm_symbol.add name;
           value_fun = sym;
           value_arg_types = Array.map (fun field -> Lm_symbol.add field.raw_object_field_type) arg_types;
           value_result_type = Lm_symbol.add result_type
         }
      in
         { info with value_fun = info }

   let build_dll objects enums values set_handler globals =
      let objects = Array.map build_object objects in
      let enums   = Array.map build_enum enums in
      let values  = Array.map build_value values in
      let info =
         { dll_objects = objects;
           dll_enums   = enums;
           dll_values  = values;
           dll_globals = globals
         }
      in
         info, set_handler

   (*
    * Open and format the DLL.
    *)
   let opendll name flags =
      match dlopen name flags with
         Export (objects, enums, values, set_handler, globals) ->
            Some (build_dll objects enums values set_handler globals)
       | NoExport ->
            None

   let info (x, _) =
      x

   (************************************************************************
    * Static version.
    *)
   external dlopen_static : dll_export -> raw_info = "lm_dlopen_static"

   let open_static info =
      match dlopen_static info with
         Export (objects, enums, values, set_handler, globals) ->
            build_dll objects enums values set_handler globals
       | NoExport ->
            raise (Failure "Lm_dll.open_static")

   let get_enum (info, _) i j =
      info.dll_enums.(i).enum_fields.(j).enum_field_value

   let get_globals (info, _) =
      info.dll_globals

   (************************************************************************
    * Set the callback handler.
    *)
   external set_handler : set_handler -> (Obj.t -> int) -> unit = "lm_set_callback_handler"

   let set_callback_handler (_, info) f =
      set_handler info f

   (************************************************************************
    * Unsafe application.
    *)
   let unsafe_get f =
      f.value_fun

   external unsafe_apply : dll_sym -> Obj.t array -> Obj.t = "lm_dlapply";;

   (*
    * Null pointers.
    *)
   external get_null : unit -> Obj.t = "lm_dlnull";;

   let null = get_null ()

   let p_NULL = Obj.magic null

   let is_null x =
      Obj.repr x == null

   external pointer_of_int : int -> Obj.t = "lm_dlpointer_of_int"
   external int_of_pointer : Obj.t -> int = "lm_dlint_of_pointer"

   (************************************************************************
    * Function calls.
    *)

   (*
    * Type casting.
    *)
   let void_of_value ty v =
      Obj.repr ()

   let int_of_value ty v =
      let v =
         match v with
            ValBool true ->
               1
          | ValUnit
          | ValBool false ->
               0
          | ValInt i ->
               i
          | ValFloat x ->
               int_of_float x
          | ValString s ->
               (try int_of_string s with
                   Failure _ ->
                      raise (TypeError (ty, v)))
          | ValObject _
          | ValAbstract _ ->
               raise (TypeError (ty, v))
      in
         Obj.repr v

   let float_of_value ty v =
      let x =
         match v with
            ValBool true ->
               1.0
          | ValUnit
          | ValBool false ->
               0.0
          | ValInt i ->
               float_of_int i
          | ValFloat x ->
               x
          | ValString s ->
               (try float_of_string s with
                   Failure _ ->
                      raise (TypeError (ty, v)))
          | ValObject _
          | ValAbstract _ ->
               raise (TypeError (ty, v))
      in
         Obj.repr x

   let string_of_value ty v =
      let s =
         match v with
            ValUnit ->
               ""
          | ValBool b ->
               string_of_bool b
          | ValInt i ->
               string_of_int i
          | ValFloat x ->
               string_of_float x
          | ValString s ->
               s
          | ValObject _
          | ValAbstract _ ->
               raise (TypeError (ty, v))
      in
         Obj.repr s

   let c_info =
      ["void",   void_of_value;
       "int",    int_of_value;
       "float",  float_of_value;
       "double", float_of_value;
       "char *", string_of_value]

   let c_table =
      List.fold_left (fun table (s, f) ->
            SymbolTable.add table (Lm_symbol.add s) f) SymbolTable.empty c_info

   let c_of_value ty v =
      try (SymbolTable.find c_table ty) ty v with
         Not_found ->
            match v with
               ValAbstract (ty', v) when Lm_symbol.eq ty' ty ->
                  Obj.repr v
             | _ ->
                  raise (TypeError (ty, v))

   (*
    * Type casting.
    *)
   let value_of_void v =
      ValUnit

   let value_of_int v =
      ValInt (Obj.magic v)

   let value_of_float v =
      ValFloat (Obj.magic v)

   let value_of_string v =
      ValString (Obj.magic v)

   let c_info =
      ["void",   value_of_void;
       "int",    value_of_int;
       "float",  value_of_float;
       "double", value_of_float;
       "char *", value_of_string]

   let c_table =
      List.fold_left (fun table (s, f) ->
            SymbolTable.add table (Lm_symbol.add s) f) SymbolTable.empty c_info

   let value_of_c ty v =
      try (SymbolTable.find c_table ty) v with
         Not_found ->
            ValAbstract (ty, v)

   (*
    * Now the application.
    *)
   let apply f args =
      let { value_fun = sym;
            value_arg_types = arg_types;
            value_result_type = result_type
          } = f
      in
      let len1 = Array.length arg_types in
      let len2 = Array.length args in
      let () =
         if len1 <> len2 then
            raise (ArityMismatch (f, len1, len2))
      in
      let args = Array.mapi (fun i v -> c_of_value (Array.unsafe_get arg_types i) v) args in
      let x = unsafe_apply sym args in
         value_of_c result_type x
end;;

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
