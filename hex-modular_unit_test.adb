------------------------------------------------------------------------------
--                                                                          --
--                   Generic HEX String Handling Package                    --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai, Ensi Martini, Aninda Poddar, Noshen Atashe               --
--    (ANNEXI-STRAYLINE)                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada;
with Ada.Text_IO;


package body Hex.Modular_Unit_Test is

   use type Subject.Modular_Value;
   
   protected type Coordinator_Type is
      procedure Enable_Reporting (Set: in Boolean);
      
      procedure Report_Start (Sequence   : in Positive;
                              First, Last: in Subject.Modular_Value);
      
      procedure Print_Info (Sequence: in Positive;
                            Message: in String);
      
      procedure Report_Failure (Sequence: in Positive;
                                Message : in String);
      
      procedure Report_Success (Sequence: in Positive);
      
      entry Wait_Finish;
      
      function Final_Result return Test_Result;
      
   private
      Result       : Test_Result := Pass;
      Reporting    : Boolean     := False;
      Engaged_Tasks: Natural     := 0;
   end Coordinator_Type;
      
   protected body Coordinator_Type is
      
      procedure Enable_Reporting (Set: in Boolean) is
      begin
         Reporting := Set;
      end Enable_Reporting;
      
      procedure Report_Start (Sequence   : in Positive;
                              First, Last: in Subject.Modular_Value)
      is
         use Ada.Text_IO;
      begin
         Engaged_Tasks := Engaged_Tasks + 1;
         
         if Reporting then
         Put_Line 
              ("[INIT] Task" & Positive'Image (Sequence)
                 & " (" 
                 & Subject.Encode (First, Upper_Case)
                 & " - " 
                 & Subject.Encode (Last, Upper_Case)
                 & ") Started.");
         end if;
         end Report_Start;
         
      procedure Print_Info (Sequence: in Positive;
                            Message : in String)
      is
         use Ada.Text_IO;
      begin
         
         if Reporting then
            Put_Line ("[INFO] Task" & Positive'Image (Sequence) 
                        & ": " & Message);
         end if;
         
      end Print_Info;
      
      
      procedure Report_Failure (Sequence: in Positive;
                                Message : in String)
      is
         use Ada.Text_IO;
      begin
         Result        := Fail;
         
         if Engaged_Tasks > 0 then
            Engaged_Tasks := Engaged_Tasks - 1;
         end if;
         
         
         if Reporting then
            Put_Line ("[FAIL] Task" & Positive'Image (Sequence) 
                        & ": " & Message);
         end if;
      end;
   
      procedure Report_Success (Sequence: in Positive) is
         use Ada.Text_IO;
      begin
         Engaged_Tasks := Engaged_Tasks - 1;
         
         if Reporting then
            Put_Line ("[PASS] Task" & Positive'Image (Sequence) & 
                        ": Complete");
         end if;
      end Report_Success;
   
      entry Wait_Finish
        when Engaged_Tasks = 0 is
      begin
         null;
      end Wait_Finish;
   
      function Final_Result return Test_Result is (Result);
   
   end Coordinator_Type;
      
   
   
   task type Tester (Coordinator: not null access Coordinator_Type) is
      entry Go (Sequence: Positive; 
                Assignment_First: in Subject.Modular_Value;
                Assignment_Last : in Subject.Modular_Value);
   end Tester;
   
   task body Tester is
      use Subject;
      
      First, Last: Modular_Value;
      ID         : Positive;
      
      Test_Value: Modular_Value;
      Test_Text : String (1 .. Max_Nibbles);
      
      Passing: Boolean;
      
      Fail_Value: Modular_Value;
      
      function Do_Test (Value   : Modular_Value;
                        Use_Case: Set_Case)
      return Boolean with Inline
      is
      begin
         Encode (Value    => Value,
                 Buffer   => Test_Text,
                 Use_Case => Use_Case);
         
         Decode (Input => Test_Text,
                 Value => Test_Value);
         
--         Coordinator.Print_Info 
--                 (ID,
--                  '(' & "N =" & Modular_Value'Image (Value) & 
--                    " Test_Value =" & Modular_Value'Image (Test_Value) &
--                    " Test_Text  = " & Test_Text & ')');
         
         return (Test_Value = Value);

      end Do_Test;
   begin
      
      accept Go (Sequence: Positive; 
                 Assignment_First: in Subject.Modular_Value;
                 Assignment_Last : in Subject.Modular_Value)
      do
         ID    := Sequence;
         First := Assignment_First;
         Last  := Assignment_Last;
         

         
         Coordinator.Report_Start (Sequence => ID,
                                   First    => First,
                                   Last     => Last);
      end Go;
      
      for N in First .. Last loop
         -- Lower_Case
         begin
            Passing := Do_Test (N, Lower_Case)
              and then Do_Test (N, Upper_Case);
         
         exception
            when e: others => 
               Coordinator.Print_Info 
                 (ID,
                  '(' & "N =" & Modular_Value'Image (Fail_Value) & 
                    " Test_Value =" & Modular_Value'Image (Test_Value) &
                    " Test_Text  = " & Test_Text & ')');
               Coordinator.Print_Info 
                 (ID,
                  "Ex: " 
                    & Exceptions.Exception_Information (e));
               Passing := False;
         end;
         
         if not Passing then
            Fail_Value := N;
            exit;
         end if;
         
--         Coordinator.Print_Info
--           (Sequence => ID,
--            Message  => "-> N =" & Modular_Value'Image (N) & 
--              " Test_Value =" & Modular_Value'Image (Test_Value) &
--              " Test_Text  = " & Test_Text);
         
      end loop;
      
      if not Passing then
         Coordinator.Report_Failure 
           (Sequence => ID,
            Message  => "N =" & Modular_Value'Image (Fail_Value) & 
              " Test_Value =" & Modular_Value'Image (Test_Value) &
              " Test_Text  = " & Test_Text);
      else
         Coordinator.Report_Success (ID);
      end if;
      
   exception
            when e: others => 
               Ada.Text_IO.Put_Line ('[' & "N =" & Modular_Value'Image (Fail_Value) & 
              " Test_Value =" & Modular_Value'Image (Test_Value) &
              " Test_Text  = " & Test_Text & ']');
               Ada.Text_IO.Put_Line ("Ex: " 
                                       & Exceptions.Exception_Information (e));
            Passing := False;
      
   end Tester;

   

   function Execute_Test 
     (First : Subject.Modular_Value := Subject.Modular_Value'First;
      Last  : Subject.Modular_Value := Subject.Modular_Value'Last;
      Tasks : Positive := 1;
      Report: Boolean  := False)
     return Test_Result
   is
      Supervisor: aliased Coordinator_Type;
      
      Task_Force: array (1 .. Tasks) of Tester(Supervisor'Access);
      
      Tester_First, Tester_Last, Module: Subject.Modular_Value;
   begin
      
      Supervisor.Enable_Reporting (Report);
      
      Module
        := (Last - First) / Subject.Modular_Value (Tasks);
      
      Tester_First := First;
      Tester_Last  := First + Module - 1;
      
      for ID in Task_Force'Range loop
         if ID = Task_Force'Last then
            Tester_Last := Last;
         end if;
         
         Task_Force(ID).Go (Sequence => ID,
                            Assignment_First => Tester_First,
                            Assignment_Last  => Tester_Last);
         
         Tester_First := Tester_Last + 1;
         Tester_Last := Tester_First + Module;
         
      end loop;
      
      
      return Supervisor.Final_Result;
      
   end Execute_Test;
   
end Hex.Modular_Unit_Test;
