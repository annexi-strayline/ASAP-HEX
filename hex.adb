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

package body Hex
  with SPARK_Mode => On
is
   pragma Assertion_Policy (Ignore);
   
   pragma Suppress (Division_Check);
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   -- Fully verified unit
   
   
   ------------------
   -- Idenfity_Hex --
   ------------------
   procedure Identify_Hex (Source: in     String;
                           First :    out Positive;
                           Last  :    out Natural)
   is
      
      procedure Search_Initial_Candidate
        (Start_From: in     Positive;
         Candidate :    out Positive;
         Found     :    out Boolean)
      with 
        Inline  => True,
        Global  => (Input => Source),
        Depends => ((Candidate, Found) => (Source, Start_From)),
        Pre     => Source'Length > 0 and Start_From in Source'Range,
        Post    => (if Found then 
                       Candidate >= Start_From
                       and Candidate in Source'Range
                       and Source (Candidate) in Hex_Character
                       and Demarcation_Precedent_Check
                         (Source => Source,
                          First  => Candidate))
      is
      begin
         -- Attempts to identify they first valid hexadecimal digit which
         -- satisfies the precedent check, starting from Start_From
         Candidate := Start_From;
         Found     := False;
         
         for I in Start_From .. Source'Last loop
            if Source(I) in Hex_Character
              and then Demarcation_Precedent_Check
                (Source => Source,
                 First  => I)
            then
               Candidate := I;
               Found := True;
               return;
            end if;
            
            pragma Loop_Invariant 
              (not Found and Candidate = Start_From);
         end loop;
      end Search_Initial_Candidate;
      
      
      procedure Search_Terminal_Candidate
        (Initial  : in     Positive;
         Candidate:    out Positive;
         Found    :    out Boolean)
      with 
        Inline => True,
        Global => (Input => Source),
        Depends => ((Candidate, Found) => (Source, Initial)),
        Pre => Source'Length >= 1 
               and then Initial in Source'Range
               and then Source(Initial) in Hex_Character
               and then Demarcation_Precedent_Check (Source => Source,
                                                     First  => Initial),
        Post => Candidate in Source'Range
                and then (if Initial = Source'Last then Found)
                and then (if Found then
                             Candidate >= Initial
                             and then Valid_Hex_String 
                               (Source (Initial .. Candidate))
                             and then Demarcation_Subsequent_Check 
                               (Source => Source,
                                Last   => Candidate)
                          else
                             Candidate > Initial
                             and then Valid_Hex_String 
                               (Source (Initial .. Candidate - 1))
                             and then Source(Candidate) not in Hex_Character)
      is
      begin
         -- Given a valid initial position of a candidate hexadecimal string,
         -- attempts to find the terminal digit of that string such that
         -- the demarcation subsequent check passes.
         
         for I in Initial .. Source'Last loop
            if Source(I) not in Hex_Character then
               -- Precondition ensures us that this never happens on the first
               -- iteration, (Initial points at a Hex_Character) therefore 
               -- I - 1 is always a Hex_Character
               
               if Demarcation_Subsequent_Check (Source => Source,
                                                Last   => I - 1)
               then
                  -- This is a good one
                  Found := True;
                  Candidate := I - 1;
                  return;
               else
                  -- No luck, but we set Candidate to be what should
                  -- be the next start-point
                  Found := False;
                  Candidate := I;
                  return;
               end if;
            end if;
            
            pragma Loop_Invariant (Source(I) in Hex_Character);
            pragma Loop_Invariant (Valid_Hex_String (Source(Initial .. I)));
            -- Used to prove our postcondition
            
         end loop;
         
         -- Based on the Loop_Invariants of the above loop, as well as
         -- the two possible "return" paths, we know that
         -- Initial .. Source'Last is valid
         Found     := True;
         Candidate := Source'Last;
         
      end Search_Terminal_Candidate;
      
      
      Trial_OK, Retrial_OK: Boolean;
      Trial_First, Retrial_First, Trial_Last: Positive;
   begin
      
      -- With the above nested procedures, the high-level view is quite simple.
      -- We continually alternate between looking for a initial candidate,
      -- then a terminal candidate, starting with Source'First, until the
      -- initial candidate attempt fails, at which point we fail to identify a
      -- string. Of course the first successful pair means a match, which is
      -- the goal
      
      if Source'Length = 0 then
         First := 1;
         Last  := 0;
         -- This is an interesting special case. 
         -- The Ada Standard allows for null ranges to be totally outside
         -- of the underlying subtype when used as an index. Therefore it
         -- is entirely possible for a String to have a range like (-3 .. -8).
         --
         -- The goal of this procedure is to avoid any precondition, so that
         -- it can be safely called from full Ada, while retaining the proven 
         -- postcondition.
         --
         -- So in the case of an input null string, we explicitly set
         -- First .. Last to a null range, but one that is not actually tied to
         -- the range of the input string.
         --
         -- We make this caveat clear in the postcondition in the
         -- specification, in the off chance that it would cause a problem
         --
         -- Realistically, a good programmer should not (cannot?) be depending
         -- on First = Source'First if Source is a null string.
         return;
      end if;
      
      pragma Assert (Source'First in Positive);
      
      Trial_Last := Source'First;
      
      loop
         Search_Initial_Candidate (Start_From => Trial_Last,
                                   Candidate  => Trial_First,
                                   Found      => Trial_OK);
         
         if not Trial_OK then
            First := Source'First;
            Last  := First - 1;
            return;
         end if;
         
         -- We need to handle skipping of any '0x' condition, but we
         if Source(Trial_First) = '0'
           and then Trial_First < Source'Last
           and then Source(Trial_First + 1) in 'x' | 'X'
         then
            -- We require one of two valid conditions.
            -- 1. Source literally starts with "0x", or
            -- 2. "0x" is preceeded by Valid_Exterior
            if Trial_First + 1 < Source'Last
              and then Source(Trial_First + 2) in Hex_Character
              and then (if Trial_First + 2 < Source'Last then
                           Source(Trial_First + 3) in Valid_Exterior)
            then
               -- We can safely skip forward and try again
               Trial_First := Trial_First + 2;
               
               -- By doing this here instead of trying to re-run the
               -- whole loop, we avoid problems with "0x0x" sequences
               -- After this search, we won't try another skip, if
               -- 0x follows, Retrial_First will be Trial_First, and then
               -- Search_Terminal_Candidate will fail
               Search_Initial_Candidate (Start_From => Trial_First,
                                         Candidate  => Retrial_First,
                                         Found      => Retrial_OK);
               
               
               if not Retrial_OK then
                  -- Game over
                  First := Source'First;
                  Last  := First - 1;
                  return;
                  
               else
                  -- In theory, Retrial_First should be equal to
                  -- Trial_First, but this ensures that we are still
                  -- meeting our contractual requirements
                  Trial_First := Retrial_First;
                  
               end if;
            end if;
            
         end if;
         
         
         Search_Terminal_Candidate (Initial   => Trial_First,
                                    Candidate => Trial_Last,
                                    Found     => Trial_OK);
         
         if Trial_OK then
            -- Match - we're done
            First := Trial_First;
            Last  := Trial_Last;
            return;
         end if;
         
         -- Otherwise, Trail_Last will be set by Search_Terminal_Candidate
         -- to be an optimal start-point for Search_Initial_Candidate
         
         pragma Loop_Invariant (Trial_First in Source'Range);
         pragma Loop_Invariant (Trial_Last  in Source'Range);
         pragma Loop_Variant   (Increases => Trial_Last);
         
      end loop;
      
      
   end Identify_Hex;
   
end Hex;
