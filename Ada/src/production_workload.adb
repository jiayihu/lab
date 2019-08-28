--  |---------------------------------------------------------------------------
--  |
--  | Unit Name:  Workload
--  |
--  | Unit Type:  Package Body
--  |
--  | Description:
--  |   See the description in the package specification and the description
--  |   of the Small_Whetstone procedure below.
--  |
--  |   The Small_Whetstone procedure requires an implementation-dependent
--  |   mathematical library.  Refer to the explanatory comments in the
--  |   procedure for details.
--  |
--  |---------------------------------------------------------------------------

-- IMPLEMENTATION-DEPENDENT library name; see examples below
-- with Float_Math_Lib;
-- use  Float_Math_Lib;

-- with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

package body Production_Workload is

  -- IMPLEMENTATION-DEPENDENT subtype definition; see comments below
  subtype Whet_Float is Float;

-- package Math is new Ada.Numerics.Generic_Elementary_Functions(Whet_Float);

-- use Math;
  -- IMPLEMENTATION-DEPENDENT library & function names; see examples in
  -- comments below
--  function Log(X : in Whet_Float) return Whet_Float
--    renames Float_Math_Lib.Log;

  --  |-------------------------------------------------------------------------
  --  |
  --  | Unit Name: Small_Whetstone
  --  |
  --  | Unit Type: Procedure Body
  --  |
  --  | This version of the Whetstone benchmark is designed to have an inner
  --  | loop which executes only 1000 Whetstone instructions.  This is so that
  --  | smaller units of CPU time can be requested for benchmarks, especially
  --  | real-time benchmarks.  The parameter "Kilo_Whets" determines the number
  --  | of loop iterations; a value of 1 means the loop will execute 1000
  --  | Whetstone Instructions. A Whetstone Instruction corresponds to about
  --  | 1.3 machine instructions on a conventional machine with floating point.
  --  |
  --  | Small_Whetstone was developed by Brian Wichmann of the UK National
  --  | Physical Laboratory (NPL).  The Ada version was translated at the
  --  | Carnegie Mellon University Software Engineering Institute from the
  --  | original standard Pascal language version (see references below).
  --  | This Hartstone version has been adapted from the Ada standard
  --  | version by making the Kilo_Whets variable a passed parameter, and
  --  | by raising an exception, rather than printing an error message, if
  --  | the benchmark's internal consistency check fails.
  --  |
  --  | Small_Whetstone uses the following mathematical functions, which are
  --  | listed here using the ISO/WG9 Numerics Rapporteur Group proposed
  --  | standard names for functions of a Generic_Elementary_Functions library
  --  | (Float_Type is a generic type definition):
  --  |
  --  |   function Cos  (X : Float_Type) return Float_Type;
  --  |   function Exp  (X : Float_Type) return Float_Type;
  --  |   function Log  (X : Float_Type) return Float_Type; -- Natural logs
  --  |   function Sin  (X : Float_Type) return Float_Type;
  --  |   function Sqrt (X : Float_Type) return Float_Type;
  --  |
  --  | The name of the actual mathematical library and the functions it
  --  | provides are implementation-dependent.  For Small_Whetstone, the
  --  | function name to be careful of is the natural logarithm function;
  --  | some vendors call it "Log" while others call it "Ln".  A renaming
  --  | declaration has been provided to rename the function according to
  --  | the ISO/WG9 name.
  --  | Another implementation-dependent area is the accuracy of floating-
  --  | point types.  One vendor's Float is another's Long_Float, or even
  --  | Short_Float.  The subtype Whet_Float is provided so that the change
  --  | can be made in a single place; users should modify it as necessary
  --  | to ensure comparability of their test runs.
  --  |
  --  | Examples of some vendor mathematical library and log function names,
  --  | and the values of the 'Digits attribute for the floating-point types
  --  | are provided in the comments below.  The ONLY changes a user should
  --  | make to run Small_Whetstone are (a) the library name, (b) the log
  --  | function name, if necessary, and (c) the definition of the subtype
  --  | Whet_Float, if necessary.  Any changes should be documented along
  --  | with reported results.
  --  |
  --  | References:
  --  |   The first two apply only to the full version of Whetstone.  The
  --  |   first includes a listing of the original Algol version.  The second
  --  |   includes an Ada listing.  The third reference also deals mostly with
  --  |   the full Whetstone benchmark but in addition contains a brief
  --  |   rationale for the Small_Whetstone benchmark and a listing of its
  --  |   standard Pascal version.
  --  |
  --  |   Curnow, H.J., and Wichmann, B.A.
  --  |     A Synthetic Benchmark
  --  |     The Computer Journal, Vol. 19, No. 1, February 1976, pp. 43-49.
  --  |
  --  |   Harbaugh, S., and Forakis, J.A.
  --  |     Timing Studies Using a Synthetic Whetstone Benchmark
  --  |     Ada Letters, Vol. 4, No. 2, 1984, pp. 23-34.
  --  |
  --  |   Wichmann, B.A.,
  --  |     Validation Code for the Whetstone Benchmark
  --  |     NPL report DITC 107/88, March 1988.
  --  |     National Physical Laboratory,
  --  |     Teddington, Middlesex TW11 OLW, UK.
  --  |
  --  |-------------------------------------------------------------------------

  ----------------------------------------------------------------------------
  -- Math library for TeleSoft TeleGen2 VAX/VMS -> MC68020:
  --
  --    with Math_Library;
  --
  --    package Math is new Math_Library(Whet_Float);
  --    use Math;
  --
  -- Natural logs (base e) = Ln(x);  base 10 logs = Log(x).
  -- There is also a pre-instantiated library called Float_Math_Library.
  --
  -- Float'Digits = 6;  Long_Float'Digits = 15
  ----------------------------------------------------------------------------
  -- Math library for Verdix VADS VAX/VMS -> MC68020:
  --
  --    with Generic_Elementary_Functions;
  --
  --    package Math is new Generic_Elementary_Functions(Whet_Float);
  --    use Math;
  --
  -- Natural logs (base e) = Log(x);  base 10 logs = Log(x, Base => 10).
  --
  -- Short_Float'Digits = 6;  Float'Digits = 15
  ---------------------------------------------------------------------------
  -- Math library for DEC VAX Ada:
  --
  --    with Float_Math_Lib;
  --    use  Float_Math_Lib;
  --
  -- Natural logs (base e) = Log(x);  base 10 logs = Log10(x).
  --
  -- Float'Digits = 6;  Long_Float'Digits = 15;  Long_Long_Float'Digits = 33
  ----------------------------------------------------------------------------
  -- Math library for Alsys Ada VAX/VMS -> MC68020:
  --
  --    with Math_Lib;
  --
  --    package Math is new Math_Lib(Whet_Float);
  --    use Math;
  --
  -- Natural logs (base e) = Log(x);  base 10 logs = Log10(x).
  --
  -- If using the 68881 Floating-Point Co-Processor, the Math_Lib_M68881
  -- package can be used.
  --
  -- Float'Digits = 6;  Long_Float'Digits = 15
  ----------------------------------------------------------------------------
  -- Math library for DDC-I Ada (DACS-80386PM) VAX/VMS -> i80386:
  --
  --    with Math_Pack;
  --    use  Math_Pack;
  --
  -- Natural logs (base e) = Ln(x);  base 10 logs = Log(x, 10.0).
  --
  -- Float'Digits = 6;  Long_Float'Digits = 15
  ----------------------------------------------------------------------------
  -- Math library for Systems Designers XD Ada VAX/VMS -> MC68020:
  --
  --    with Float_Math_Lib;
  --    use  Float_Math_Lib;
  --
  -- Natural logs (base e) = Log(x);  base 10 logs = Log10(x).
  --
  -- Float'Digits = 6;  Long_Float'Digits = 15;  Long_Long_Float'Digits = 18
  ----------------------------------------------------------------------------

  procedure Small_Whetstone(Kilo_Whets : in Positive) is

    T  : constant := 0.499975; -- Values from the original Algol
    T1 : constant := 0.50025;  --   Whetstone program and the
    T2 : constant := 2.0;      --     Pascal SmallWhetstone program

    N8 : constant := 10;       -- Loop iteration count for module 8
    N9 : constant :=  7;       -- Loop iteration count for module 9

    Value     : constant := 0.941377; -- Value calculated in main loop
    Tolerance : constant := 0.00001;  -- Determined by interval arithmetic

    I   : Integer;
    IJ  : Integer := 1;
    IK  : Integer := 2;
    IL  : Integer := 3;

    Y   : constant Whet_Float := 1.0; -- Constant within loop
    Z   : Whet_Float;
    Sum : Whet_Float := 0.0; -- Accumulates value of Z

    subtype Index is Integer range 1..N9; -- Was a type in the Pascal version
    E1  : array (Index) of Whet_Float;

      procedure Clear_Array is
      begin
         for Loop_Var in E1'Range loop
            E1(Loop_Var) := 0.0;
         end loop;
      end Clear_Array;

      procedure P0 is
      begin
         if (IJ < 1) or (IK < 1) or (IL < 1) then
            Ada.Text_IO.Put_Line ("Parameter error 1 at line 216");
            IJ := 1; IK := 1; IL := 1;
         elsif (IJ > N9) or (IK > 9) or (IL > N9) then
            Ada.Text_IO.Put_Line ("Parameter error 2 at line 216");
            IJ := N9; IK := N9; IL := N9;
         end if;
         E1(IJ) := E1(IK);
         E1(IK) := E1(IL);
         E1(I)  := E1(IJ);
      end P0;

    procedure P3(X : Whet_Float;
                 Y : Whet_Float;
                 Z : out Whet_Float) is
      Xtemp: constant Whet_Float := T * (Z + X);
      Ytemp: constant Whet_Float := T * (Xtemp + Y);
    begin
      Z := (Xtemp + Ytemp) / T2;
    end P3;

  begin -- Small_Whetstone

    for Outer_Loop_Var in 1..Kilo_Whets loop

      Clear_Array;

      -- Module 6: Integer arithmetic

         IJ := (IK - IJ) * (IL - IK);
         IK := IL - (IK - IJ);
         IL := (IL - IK) * (IK + IL);
         if (IK - 1) < 1 or (IL -1) < 1 then
            Ada.Text_IO.Put_Line ("Parameter error 3 at line 244");
         elsif (IK - 1) > N9 or (IL - 1) > N9 then
            Ada.Text_IO.Put_Line ("Parameter error 4 at line 244");
         else
            E1(IL - 1) := Whet_Float(IJ + IK + IL);
            E1(IK - 1) := Sin( Whet_Float(IL) );
         end if;

      -- Module 8: Procedure calls

      Z := E1(4);
      for Inner_Loop_Var in 1..N8 loop
        P3( Y * Whet_Float(Inner_Loop_Var), Y + Z, Z );
      end loop;

      -- Second version of Module 6:

         IJ := IL - (IL - 3) * IK;
         IL := (IL - IK) * (IK - IJ);
         IK := (IL - IK) * IK;
         if (IL - 1) < 1 then
            Ada.Text_IO.Put_Line ("Parameter error 5 at line 264");
         elsif (IL -1) > N9 then
            Ada.Text_IO.Put_Line ("Parameter error 6 at line 264");
         else
            E1(IL - 1) := Whet_Float(IJ + IK + IL);
         end if;

         if (IK + 1) > N9 then
            Ada.Text_IO.Put_Line ("Parameter error 7 at line 272");
         elsif (IK + 1) < 1 then
            Ada.Text_IO.Put_Line ("Parameter error 8 at line 272");
         else
            E1(IK + 1) := Abs( Cos(Z) );
         end if;

      -- Module 9: Array references

      -- Note: In the Pascal version, the global variable I is used as both
      --       the control variable of the for loop and an array index
      --       within procedure P0.  Because the for-loop control variable
      --       of Ada is strictly local, this translation uses a while loop.

      --  I := 1;
      --  while I <= N9 loop
      --    P0;
      --    I := I + 1;
      --  end loop;

      -- Module 11: Standard mathematical functions

      -- Note: The actual name of the natural logarithm function used here
      --       is implementation-dependent.  See the comments above.

      Z := Sqrt( Exp( Log(E1(N9)) / T1 ) );

      Sum := Sum + Z;

      -- Check the current value of the loop computation

      if abs(Z - Value) > Tolerance then
        Sum := 2.0 * Sum; -- Forces error at end
        IJ := IJ + 1;     -- Prevents optimization
      end if;

    end loop;

    -- Self-validation check

    --  if abs( Sum / Whet_Float(Kilo_Whets) - Value ) >
    --   Tolerance * Whet_Float(Kilo_Whets) then
    --    raise Workload_Failure;
    --  end if;

   exception
      when Error : others =>
         --  Ada.Text_IO.Put_Line
         --    ("Something has gone wrong here: " & 
		 --     Exception_Information (Error));
		 null;
   end Small_Whetstone;

end Production_Workload;
