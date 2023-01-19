with Ada.Text_IO; use Ada.Text_IO;

with GNAT.AWK;
with GNAT.Regpat;

procedure Day_2 is

  package Choices is
    -- Order of enums is crucial, do not change.
    -- They all follow the pattern Rock-Paper-Scissors/Loss-Draw-Win when applicable.

    type Opponent is (A, B, C);
    type Encrypted is (X, Y, Z);
    type Pure is (Rock, Paper, Scissors);
    for Pure use (Rock => 1, Paper => 2, Scissors => 3);
    function From_String (S : in String) return Pure;

    type Outcome is (Loss, Draw, Win);
    for Outcome use (Loss => 0, Draw => 3, Win => 6);
    function From_String (S : in String) return Outcome;

    function Cheat
     (Opponent : in Pure; Outcome : in Choices.Outcome) return Pure;
  end Choices;

  package body Choices is
    function From_String (S : in String) return Outcome is
      Encrypted : constant Choices.Encrypted := Choices.Encrypted'Value (S);
      Encrypted_Pos : constant Natural := Choices.Encrypted'Pos (Encrypted);
      Outcome       : constant Choices.Outcome   :=
       Choices.Outcome'Val (Encrypted_Pos);
    begin
      return Outcome;
    end From_String;

    function From_String (S : in String) return Pure is
      Opponent     : constant Choices.Opponent := Choices.Opponent'Value (S);
      Opponent_Pos : constant Natural := Choices.Opponent'Pos (Opponent) mod 3;
      Choice       : constant Pure := Choices.Pure'Val (Opponent_Pos);
    begin
      return Choice;
    end From_String;

    function Cheat
     (Opponent : in Pure; Outcome : in Choices.Outcome) return Pure is
      Outcome_Choice_Offsets : constant array (Loss .. Win) of Integer :=
       [-1, 0, 1];
      ------------------
      Opponent_Pos           : constant Natural := Pure'Pos (Opponent);
      Choice_Pos             : constant Natural                        :=
       (Opponent_Pos + Outcome_Choice_Offsets (Outcome)) mod 3;
    begin
      return Choices.Pure'Val (Choice_Pos);
    end Cheat;
    -- The basic idea is for opponent choice C the player may:
    -- * Win  with C - 1
    -- * Draw with C + 0
    -- * Loss with C + 1
    -- Unfortunatelly we cannot map negative numbers in Ada enums, so we simulate it by
    -- adding the Pos of the outcome to the C. Thus for Outcome O it becomes:
    -- * Win  with (C + Win)  mod 3 = (C + 2) mod 3
    -- * Draw with (C + Draw) mod 3 = (C + 0) mod 3
    -- * Loss with (C + Loss) mod 3 = (C + 1) mod 3
    -- Use of mod 3 is to prevent Constraint_Error and wrap the position around the enum.
  end Choices;

  ------------------------------
  -- Processing
  ------------------------------

  package AWK renames GNAT.AWK;
  package Regpat renames GNAT.Regpat;

  Total_Score : Natural := 0;
  procedure Action is
    Opponent : constant Choices.Pure    := Choices.From_String (AWK.Field (1));
    Outcome : constant Choices.Outcome := Choices.From_String (AWK.Field (2));
    Outcome_Value : constant Natural := Choices.Outcome'Enum_Rep (Outcome);
    Player        : constant Choices.Pure := Choices.Cheat (Opponent, Outcome);
    Choice_Value  : constant Natural         := Choices.Pure'Enum_Rep (Player);
    Score         : constant Natural         := Choice_Value + Outcome_Value;
  begin
    Total_Score := @ + Score;
  end Action;

begin
  AWK.Register
   (Field  => 0, Pattern => Regpat.Compile ("[A-C] [X-Z]"),
    Action => Action'Unrestricted_Access);
  AWK.Parse (AWK.Use_Current, "input/day_2.txt");

  Put_Line ("Final score: " & Total_Score'Image);
end Day_2;
