--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;

use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
    subtype Produced_Integer is Integer range 0..20;
    -- type Storage is array(Positive range <>) of Produced_Integer;
    package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Produced_Integer);

    use Integer_Vectors;
    use type Ada.Containers.Count_Type;

    package Random_Discrete is new Ada.Numerics.Discrete_Random(Produced_Integer);

    Message: constant String := "Process communication";

	task buffer is
	    entry Can_Push(Value: out Boolean);
        entry Push(Value: in Produced_Integer);
        entry Can_Pop(Value: out Boolean);
        entry Pop(Value: out Produced_Integer);
        entry Stop;
	end buffer;

	task producer is
        entry Start;
        entry Stop;
	end producer;

	task consumer is
        -- Nothing Needed, consumer is controlling the other tasks
	end consumer;

	task body buffer is
		Message     : constant String  := "buffer executing";
        Internal    : Integer_Vectors.Vector;
	begin
		Put_Line(Message);
		loop
            select
                accept Can_Push(Value: out Boolean) do
                    Value := Internal.Length < 10;
                end Can_Push;
            or
                accept Can_Pop(Value: out Boolean) do
                    Value := Internal.Length > 0;
                end Can_Pop;
            or
                when Internal.Length < 10 =>
                    accept Push(Value: in Produced_Integer) do
                        Internal.Append(Value);
                    end Push;
            or
                when Internal.Length > 0 =>
                    accept Pop(Value: out Produced_Integer) do
                        Value := Internal.Last_Element;
                        Internal.Delete_Last;
                    end Pop;
            or
                accept Stop;
                exit;
            or
                terminate;
            end select;
		end loop;
	end buffer;

	task body producer is
		Message       : constant String := "producer executing";
		Float_Gen     : Ada.Numerics.Float_Random.Generator;
		Discrete_Gen  : Random_Discrete.Generator;
		Random_Delay  : Duration;
		Random_Int    : Produced_Integer;
		Can_Push      : Boolean := False;
	begin
		Put_Line(Message);
		Random_Discrete.Reset(Discrete_Gen);
		Ada.Numerics.Float_Random.Reset(Float_Gen);
		accept Start;

		loop
            select
                accept Stop;
                exit;
            else
                Random_Delay := Duration(0.2 * Ada.Numerics.Float_Random.Random(Float_Gen));
     			delay Random_Delay;

     			Random_Int := Random_Discrete.Random(Discrete_Gen);
     			loop
     			    buffer.Can_Push(Can_Push);
     			    exit when Can_Push;
     			end loop;
     			buffer.Push(Random_Int);
            end select;
		end loop;
	end producer;

	task body consumer is
		Message       : constant String := "consumer executing";
		Float_Gen     : Ada.Numerics.Float_Random.Generator;
		Random_Delay  : Duration;
		Can_Pop       : Boolean := False;
		Sum           : Integer := 0;
		Popped        : Produced_Integer;
	begin
	    Ada.Numerics.Float_Random.Reset(Float_Gen);
		Put_Line(Message);
		producer.Start;

		Main_Loop:
		loop
            Random_Delay := Duration(2.0 * Ada.Numerics.Float_Random.Random(Float_Gen));
            delay Random_Delay;

		    loop
				buffer.Can_Pop(Can_Pop);
				exit when Can_Pop;
			end loop;

			loop
			    buffer.Can_Pop(Can_Pop);
				exit when not Can_Pop;

				buffer.Pop(Popped);
				Sum := Sum + Popped;

				if Sum > 100 then
				    Put_Line("[Success] Consumer: Sum = " & Integer'Image(Sum));
				    exit Main_Loop;
				end if;
			end loop;

			Put_Line("[Info] Consumer: Sum = " & Integer'Image(Sum));
			Sum := 0;

		end loop Main_Loop;

		producer.Stop;
		buffer.Stop;

	exception
		  when TASKING_ERROR => Put_Line("Buffer finished before producer");
		Put_Line("Ending the consumer");
	end consumer;
begin
	Put_Line(Message);
end comm1;
