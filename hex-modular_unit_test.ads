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

with Hex.Modular_Codec;

generic
   with package Subject is new Hex.Modular_Codec (<>);
package Hex.Modular_Unit_Test is
   
   type Test_Result is (Pass, Fail);
   
   function Execute_Test 
     (First : Subject.Modular_Value := Subject.Modular_Value'First;
      Last  : Subject.Modular_Value := Subject.Modular_Value'Last;
      Tasks : Positive := 1;
      Report: Boolean  := False)
     return Test_Result;
   -- Executes a unit test on a generic instantiation of Modular_Codec.
   --
   -- Each task tests a range of Modular_Value from First .. Last (inclusive),
   -- testing encoding and decoding of each value in both lower and upper
   -- cases.
   --
   -- If Report is True, details about the status and any errors are output
   -- as the test progresses
   --
   -- If Tasks is > 1, separate Tasks are spawned with the work divided as
   -- evenly as possible between them
   --
   -- This will eventually be replaced with "for parallel"
   
     
end Hex.Modular_Unit_Test;
