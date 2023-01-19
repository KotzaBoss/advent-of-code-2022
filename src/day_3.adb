with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Bounded_Vectors;
with Ada.Strings.Bounded;
with Ada.Assertions;         use Ada.Assertions;

with GNAT.AWK;

procedure Day_3 is

  type Item is
   (None, 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C',
    'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');
  Invalid_Item : constant Item := None;
  function Item_Value (I : in Item) return Natural is (Item'Enum_Rep (I));

  --------------
  -- Compartment
  --------------

  package Bounded_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
   (Max => 64);
  use Bounded_Strings;
  -------------------------------

  subtype Compartment is Bounded_String;

  function Find_Duplicate (Left, Right : in Bounded_String) return Item is
  begin
    for Item_Left of Slice (Left, 1, Length (Left)) loop
      for Item_Right of Slice (Right, 1, Length (Right)) loop
        if Item_Left = Item_Right then
          return
           Item'Value ("'" & Item_Left & "'"); -- There has to be a better way
        end if;
      end loop;
    end loop;
    return Invalid_Item;
  end Find_Duplicate;

  -----------
  -- Rucksack
  -----------

  type Rucksack is record
    Left, Right : Compartment;
    Duplicate   : Item;
  end record;

  function Make_Rucksack (Line : in String) return Rucksack is
    Left      : constant Bounded_String :=
     To_Bounded_String (Line (Line'First .. Line'Length / 2));
    Right     : constant Bounded_String :=
     To_Bounded_String (Line ((Line'Length / 2 + 1) .. Line'Last));
    Dublicate : constant Item           := Find_Duplicate (Left, Right);
  begin
    Assert (Dublicate /= Invalid_Item, "Duplicate wasnt found");
    return (Left => Left, Right => Right, Duplicate => Dublicate);
  end Make_Rucksack;

  function Tie_Compartments (R : in Rucksack) return Compartment is
   (R.Left & R.Right);

  ----------
  -- Vectors
  ----------

  package Rucksack_Vectors is new Ada.Containers.Bounded_Vectors
   (Index_Type => Natural, Element_Type => Rucksack);
  use type Ada.Containers.Count_Type;
  use Rucksack_Vectors;

  --------------
  -- Item Suming
  --------------

  Individual_Sum, Group_Sum : Natural := 0;

  subtype Item_Search_Result is Boolean;
  type Item_Search_Result_Access is access all Item_Search_Result;

  task type Item_Search_Task is
    entry Go
     (I : in Item; C : in Compartment; R : in Item_Search_Result_Access);
  end Item_Search_Task;

  task body Item_Search_Task is
    Searched_Item : Item                      := Invalid_Item;
    Result        : Item_Search_Result_Access := null;
    Compartments  : Compartment;
  begin
    accept Go
     (I : in Item; C : in Compartment; R : in Item_Search_Result_Access)
    do
      Searched_Item := I;
      Result        := R;
      Compartments  := C;
    end Go;

    declare
      Searched_String : constant String :=
       Slice (To_Bounded_String (Searched_Item'Image), 2, 2);
      --                  123
      -- Item'Image   -> "'a'"
      -- Slice (2, 2) -> "a"
    begin
      if 0 < Index (Compartments, Searched_String) then
        Result.all := True;
      end if;
    end;
  end Item_Search_Task;

  function Find_Group_Item (V : in Rucksack_Vectors.Vector) return Item with
   Pre => Length (V) = 3, Post => Find_Group_Item'Result /= Invalid_Item
  is
    Results :
     array (V.First_Index .. V.Last_Index) of aliased Item_Search_Result :=
     [others => False];
  begin
    for It in Item'Succ (None) .. Item'Last loop
      declare
        Tasks : array (V.First_Index .. V.Last_Index) of Item_Search_Task;
      begin
        for R in V.Iterate loop
          declare
            Index : constant Natural := To_Index (R);
          begin
            Tasks (Index).Go
             (It, Tie_Compartments (Element (R)),
              Results (Index)'Unchecked_Access);
          end;
        end loop;
      end;

      if (for all Result of Results => Result) then
        return It;
      else
        Results := [others => False];
      end if;
    end loop;

    return Invalid_Item;
  end Find_Group_Item;
  -- Overkill solution but fun practice with tasks

  ------------------------------
  -- Processing
  ------------------------------

  package AWK renames GNAT.AWK;

  Group_Vector : Rucksack_Vectors.Vector (3);

  procedure Action (Quit : in out Boolean) is
    pragma Unreferenced (Quit);

    Line : constant String   := AWK.Field (0);
    R    : constant Rucksack := Make_Rucksack (Line);
  begin
    Assert (Line'Length mod 2 = 0);

    Individual_Sum := @ + Item_Value (R.Duplicate);

    Group_Vector.Append (R);
    if Group_Vector.Length = 3 then
      declare
        Group_Item : constant Item := Find_Group_Item (Group_Vector);
      begin
        Group_Sum := @ + Item_Value (Group_Item);
        Group_Vector.Clear;
      end;
    end if;
  end Action;

  procedure Parse_Rucksacks is new AWK.For_Every_Line_Current_Session (Action);
begin
  Parse_Rucksacks
   (Separators => AWK.Use_Current, Filename => "input/day_3.txt");

  Put_Line
   ("Individual:" & Individual_Sum'Image & LF & "Group:" & Group_Sum'Image);
end Day_3;
