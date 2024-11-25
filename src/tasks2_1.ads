with Ada.Real_Time;       use Ada.Real_Time;
-- Add required sensor and actuator package --

package tasks2_1 is
  procedure Background;
private

  --  Define periods and times  --
  Period_Display10m : Time_Span := Milliseconds(10); 
  Time_Zero      : Time := Clock;
      
  --  Other specifications  --
  
  type EventID is (Idle, UpButtonPressed, UpButtonReleased, DownButtonPressed, DownButtonReleased, 
  LeftButtonPressed, LeftButtonReleased, RightButtonPressed, RightButtonReleased);

end tasks2_1;
