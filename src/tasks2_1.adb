with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body tasks2_1 is

  ----------------
  -- Protected Object Event --
  ----------------
  protected Event is
     entry Wait (id : out EventID);
     procedure Signal (id : in EventID);
     function Is_Signalled return Boolean;
     function Get_Current_ID return EventID;
  private
     pragma Priority (4);
     current_id : EventID := Idle; -- Default to Idle
     signalled  : Boolean := false;
  end Event;

  protected body Event is
     entry Wait (id : out EventID) when signalled is
     begin
        id := current_id;
        signalled := false;
     end Wait;

     procedure Signal (id : in EventID) is
     begin
        current_id := id;
        signalled := true;
     end Signal;

     function Is_Signalled return Boolean is
     begin
        return signalled;
     end Is_Signalled;

     function Get_Current_ID return EventID is
     begin
        return current_id;
     end Get_Current_ID;
  end Event;

  ----------------
  -- EventDispatcherTask --
  ----------------
  task EventDispatcherTask is
     pragma Priority (2);
  end EventDispatcherTask;

  task body EventDispatcherTask is
     last_event      : EventID := Idle;
     Next_Time       : Time := Clock;
  begin
     loop
     --Put_Line("EventDispatcherTask started.");
        if button_pressed(UpButton) and (last_event /= UpButtonPressed) then
         Put_Line("Up Button Pressed");
           Event.Signal(UpButtonPressed);
           Put_Line("Up Button Pressed");
           last_event := UpButtonPressed;
        elsif button_pressed(DownButton) and (last_event /= DownButtonPressed) then
           Event.Signal(DownButtonPressed);
           Put_Line("Down Button Pressed");
           last_event := DownButtonPressed;
        elsif button_pressed(RightButton) and (last_event /= RightButtonPressed) then
           Event.Signal(RightButtonPressed);
           Put_Line("Right Button Pressed");
           last_event := RightButtonPressed;
        elsif button_pressed(LeftButton) and (last_event /= LeftButtonPressed) then
           Event.Signal(LeftButtonPressed);
           Put_Line("Left Button Pressed");
           last_event := LeftButtonPressed;
        elsif not button_pressed(UpButton) and (last_event = UpButtonPressed) then
           Event.Signal(UpButtonReleased);
           Put_Line("Up Button Released");
           last_event := UpButtonReleased;
        elsif not button_pressed(DownButton) and (last_event = DownButtonPressed) then
           Event.Signal(DownButtonReleased);
           Put_Line("Down Button Released");
           last_event := DownButtonReleased;
        elsif not button_pressed(RightButton) and (last_event = RightButtonPressed) then
           Event.Signal(RightButtonReleased);
           Put_Line("Right Button Released");
           last_event := RightButtonReleased;
        elsif not button_pressed(LeftButton) and (last_event = LeftButtonPressed) then
           Event.Signal(LeftButtonReleased);
           Put_Line("Left Button Released");
           last_event := LeftButtonReleased;

        end if;

        Next_Time := Next_Time + Period_Display10m;
        delay until Next_Time;

        exit when simulation_stopped;
     end loop;
  end EventDispatcherTask;

  ----------------
  -- MotorControlTask --
  ----------------
  task MotorControlTask is
     pragma Priority (3);
  end MotorControlTask;

  task body MotorControlTask is
  begin
     loop
     --Put_Line("Motor Ccontrol Task started.");
        if Event.Is_Signalled then
           case Event.Get_Current_ID is
              when UpButtonPressed =>
                 set_motor_speed(LeftMotor, -400);
                 set_motor_speed(RightMotor, -400);
              when DownButtonPressed =>
                 set_motor_speed(LeftMotor, 400);
                 set_motor_speed(RightMotor, 400);
              when RightButtonPressed =>
                 set_motor_speed(RightMotor, 0);
                 set_motor_speed(LeftMotor, 400);
              when LeftButtonPressed =>
                 set_motor_speed(RightMotor, 400);
                 set_motor_speed(LeftMotor, 0);
              when UpButtonReleased | DownButtonReleased | RightButtonReleased | LeftButtonReleased =>
                 set_motor_speed(LeftMotor, 0);
                 set_motor_speed(RightMotor, 0);
              when others =>
                 Put_Line("Unknown Event");
           end case;
        end if;

        exit when simulation_stopped;
     end loop;
  end MotorControlTask;

  ----------------
  -- Background Procedure --
  ----------------
  procedure Background is
  begin
     while not simulation_stopped loop
        delay 0.25; -- Prevents busy waiting
     end loop;
  end Background;

end tasks2_1;
