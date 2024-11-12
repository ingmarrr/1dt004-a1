-- Cyclic scheduler

with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic is
    Message: constant String := "Cyclic scheduler";
    -- change/add your declarations here
    d: Duration := 1.0;
    d1: Duration := 0.5;
	Start_Time: Time := Clock;
	Next_Time: Time := Start_Time + d1;
	s: Integer := 0;
        

	procedure f1 is 
		Message: constant String := "f1 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f1;

	procedure f2 is 
		Message: constant String := "f2 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f2;

	procedure f3 is 
		Message: constant String := "f3 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f3;

begin
	Put_Line(Message);

	loop
		-- change/add your code inside this loop   
		f1;
		f2;
		delay until Next_Time;
		Next_Time := Next_Time + d1;
		f3;
		delay until Next_Time;
		Next_Time := Next_Time + d1;

		s := s + 1;
		exit when s > 10;
	end loop;
end cyclic;

