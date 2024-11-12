with Ada.Calendar;
with Ada.Text_IO;
use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic is
    Message : constant String := "Cyclic scheduler";
	Start_Time : Time := Clock;
	Delta_F1   : constant Duration := 1.0;
	Delta_F3   : constant Duration := 2.0;

	Next_F1    : Time := Start_Time;
	Next_F3    : Time := Start_Time + 0.5;

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
        loop
            delay until Next_F1;
            f1;
            f2;
            delay until Next_F1 + 0.5;
            Next_F1 := Next_F1 + Delta_F1;

            if Clock >= Next_F3 then
                f3;
                Next_F3 := Next_F3 + Delta_F3;
            end if;
        end loop;
end cyclic;
