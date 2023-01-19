with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

with GNAT.AWK;

procedure Day_1 is
        subtype Calories is Natural;

        type Top_3_Calories_Array is array (0 .. 2) of Calories;
        type Top_3_Calories_Record is record
                Top_3 : Top_3_Calories_Array := [others => 0];
                Sum   : Calories             := 0;
        end record;

        -- Insertion sort
        procedure Store_If_In_Top_3
               (Self : in out Top_3_Calories_Record; C : in Calories) is
        begin
                for I in Self.Top_3'Range loop
                        if C > Self.Top_3 (I) then
                                declare
                                        -- Delta is what what comes - what leaves
                                        Calories_Delta : constant Calories :=
                                               C - Self.Top_3
                                                      (Self.Top_3'Last);
                                begin
                                        Self.Sum := @ + Calories_Delta;
                                        -- Shift 1 to the right
                                        for J in reverse
                                               I + 1 .. Self.Top_3'Last
                                        loop
                                                Self.Top_3 (J) :=
                                                       Self.Top_3 (J - 1);
                                        end loop;
                                        Self.Top_3 (I) := C;
                                        exit;
                                end;
                        end if;
                end loop;
        end Store_If_In_Top_3;

        package Calories_Vectors is new Ada.Containers.Vectors
               (Index_Type => Natural, Element_Type => Calories);
        use Calories_Vectors;

        function Sum_Calories
               (V : in Calories_Vectors.Vector) return Calories is
                Sum : Calories := 0;
        begin
                for C of V loop
                        Sum := @ + C;
                end loop;
                return Sum;
        end Sum_Calories;

        ------------------------------
        -- Processing
        ------------------------------

        package AWK renames GNAT.AWK;

        Top_3_Calories  : Top_3_Calories_Record;
        Calories_Vector : Calories_Vectors.Vector;

        procedure Action (Quit : in out Boolean) is
                pragma Unreferenced (Quit);

                Line : constant String := AWK.Field (0);
        begin
                if Line'Size /= 0 then
                        Calories_Vector.Append (Calories'Value (Line));
                else
                        declare
                                Sum : constant Calories :=
                                       Sum_Calories (Calories_Vector);
                        begin
                                Store_If_In_Top_3 (Top_3_Calories, Sum);
                                Calories_Vector.Clear;
                        end;
                end if;
        end Action;

        procedure Parse_Calories is new AWK.For_Every_Line_Current_Session
               (Action);

begin
        Parse_Calories
               (Separators => AWK.Use_Current, Filename => "input/day_1.txt");

        New_Line;
        Put_Line ("Finally Top_3_Calories: " & Top_3_Calories'Image);
end Day_1;
