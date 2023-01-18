with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Assertions; use Ada.Assertions;
with GNAT.AWK;

procedure Day_4 is

   ---------
   -- Groups
   ---------

   subtype Valid_Range is Positive range 1 .. 99;

   type Responsibility_Record is record
      Low, High : Valid_Range;
   end record;

   function Make_Responsibility_Record (Line : in String) return Responsibility_Record is
      Hyphen_Index : constant Natural := Index (Line, "-");
   begin
      Assert (Hyphen_Index /= 0, "No hyphen in line");
      return (Low  => Valid_Range'Value (Line (Line'First       .. Hyphen_Index - 1)),
              High => Valid_Range'Value (Line (Hyphen_Index + 1 .. Line'Last       ))
             );
   end Make_Responsibility_Record;

   ------------------

   type Group_Record is record
      First, Second : Responsibility_Record;
   end record;

   function Make_Group_Record (Left, Right : in String) return Group_Record
   is
     (First  => Make_Responsibility_Record(Left),
      Second => Make_Responsibility_Record(Right)
     );

   ------------------------------
   -- Processing
   ------------------------------

   package AWK renames GNAT.AWK;

   package Counts is
      Overlapping            : Natural := 0;
      Completely_Overlapping : Natural := 0;
   end Counts;

   procedure Action (Quit : in out Boolean) is
      pragma Unreferenced (Quit);

      Group    : constant Group_Record          := Make_Group_Record (AWK.Field(1), AWK.Field(2));
   begin
      if Group.First.Low > Group.Second.High or else Group.First.High < Group.Second.Low
         -- Let fl = first low, fh = first high and same for second
         -- and check if either true:
         --         fl..fh
         -- sl..sh
         ------------------
         -- fl..fh
         --         sl..sh
      then
         return;
      else
         Counts.Overlapping := @ + 1;

         if (Group.First.Low <= Group.Second.Low and Group.Second.High <= Group.First.High)
           or else (Group.Second.Low <= Group.First.Low and Group.First.High <= Group.Second.High)
           -- If either is shadowed completely by the other
         then
            Counts.Completely_Overlapping := @ + 1;
         end if;
      end if;
   end Action;

   procedure For_Every_Line is new AWK.For_Every_Line_Current_Session (Action);
begin
   For_Every_Line (Separators => ",",
                   Filename => "input/day_4.txt");

   Put_Line (Counts.Overlapping'Image & Counts.Completely_Overlapping'Image);
end Day_4;
