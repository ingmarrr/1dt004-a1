--Cyclic scheduler with a watchdog: 
with Ada.Numerics.Float_Random;
with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic_wd is
    Message     : constant String := "Cyclic scheduler with watchdog";
    Start_Time : Time := Clock;
    Gen        : Ada.Numerics.Float_Random.Generator;
    Max_Duration    : constant Duration := 0.5;

    Next_F1    : Time := Start_Time;
    Next_F3    : Time := Start_Time + Max_Duration;

    Delta_F1   : constant Duration := 1.0;
    Delta_F3   : constant Duration := 2.0;


    procedure f1 is
    begin
        Put_Line("f1 executing, time is " & Duration'Image(Clock - Start_Time));
    end f1;

    procedure f2 is
    begin
        Put_Line("f2 executing, time is " & Duration'Image(Clock - Start_Time));
    end f2;

    procedure f3 is
        Random_Delay : Duration;
    begin
        Put_Line("f3 executing, time is " & Duration'Image(Clock - Start_Time));
        Random_Delay := Duration(1.0 * Ada.Numerics.Float_Random.Random(Gen));
        delay Random_Delay;
        Put_Line("f3 completed, time is " & Duration'Image(Clock - Start_Time));
        Scheduler.Stop;
    end f3;

    protected Scheduler is
        procedure Start;
        procedure Stop;
        procedure Sync;
        function Deadline_Missed return Boolean;
    private
        Done            : Boolean;
        Start_Time      : Time;
    end Scheduler;

    protected body Scheduler is
        procedure Start is
        begin
            Done := False;
            Start_Time := Clock;
        end Start;

        procedure Stop is
        begin
            Done := True;
        end Stop;

        procedure Sync is
            Current         : Time;
            Seconds_Part    : Integer;
            Next_Second     : Time;
            Next_Even       : Time;
        begin
            loop
                exit when Done;
            end loop;

            Current         := Clock;
            Seconds_Part    := Integer(Seconds(Current));
            Next_Second     := Current;
            Next_Even       := Next_Second;

            if Seconds_Part mod 2 /= 0 then
                Next_Even := Next_Even + 1.0;
            end if;

            Next_F1 := Next_Second;
            Next_F3 := Next_Even + 0.5;
        end Sync;

        function Deadline_Missed return Boolean is
        begin
            return Done and then (Clock - Start_Time) > Max_Duration;
        end Deadline_Missed;

    end Scheduler;

    task Watchdog;

    task body Watchdog is
    begin
        loop
            delay 0.1;
            if Scheduler.Deadline_Missed then
                Put_Line("[warning] F3 missed deadline");
            end if;
        end loop;
    end Watchdog;

begin
    Ada.Numerics.Float_Random.Reset(Gen);
    Put_Line(Message);

    loop
        delay until Next_F1;
        f1;
        f2;

        delay until Next_F1 + Max_Duration;
        Next_F1 := Next_F1 + Delta_F1;

        --  if Clock >= Next_F3 then
            Scheduler.Start;
            f3;
            if Scheduler.Deadline_Missed then
                Scheduler.Sync;
            else
                Next_F3 := Next_F3 + Delta_F3;
            end if;
        end if;

    end loop;
end cyclic_wd;
>>>>>>> 055c3f5 (feat: part 2, start)
