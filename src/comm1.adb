--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;

use type Ada.Containers.Count_Type;
use Ada.Numerics.Float_Random;
use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
    Message: constant String := "Process communication";
	package Integer_Vectors is 
				new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);
	subtype Vector is Integer_Vectors.Vector;

	subtype PInteger is Integer range 0..20;
	package D_Rand is new Ada.Numerics.Discrete_Random(PInteger);

	task buffer is
            -- add your task entries for communication 
		entry Insert (i: in PInteger);
		entry Remove (i: out PInteger);
		entry Producer_Turn(Producer_Flag: out Boolean);
		entry Consumer_Turn(Consumer_Flag: out Boolean);
		entry stop;
	end buffer;

	task producer is
			entry stop;
	end producer;

	task consumer is
            -- add your task entries for communication 
	end consumer;

task body buffer is
	  Producer_Flag : Boolean := False;
	  Consumer_Flag : Boolean := False;
	  Shared_Buffer : Vector := Integer_Vectors.Empty_Vector;
   begin
      loop
         select
			accept Producer_Turn(Producer_Flag: out Boolean) do
				Producer_Flag := (Shared_Buffer.Length) < 11;
			end Producer_Turn;
		or
			accept Consumer_Turn(Consumer_Flag: out Boolean) do
				Consumer_Flag := (Shared_Buffer.Length) > 0;
			end Consumer_Turn;
		or 
			when (Shared_Buffer.Length) < 11 =>
               accept Insert (I : in PInteger) do
                  Shared_Buffer.Append (I);
                  Producer_Flag := False;
               end Insert;
		or 
			when (Shared_Buffer.Length) > 0 =>
               accept Remove (I : out PInteger) do
                  I := Shared_Buffer.First_Element;
                  Shared_Buffer.Delete_First;
                  Consumer_Flag := False;
               end Remove;
		or
			accept stop;
			exit;
		end select;
      end loop;
   end buffer;

	task body producer is 
		Message: constant String := "producer executing";
		D_Gen: D_Rand.Generator;
		F_Gen: Ada.Numerics.Float_Random.Generator;
		Value : PInteger;
        Delay_Time : Duration;
		Producer_Flag: Boolean := False;
	begin
		Put_Line(Message);
		Ada.Numerics.Float_Random.Reset(F_Gen);
		D_Rand.Reset(D_Gen);
		loop
		select
			accept stop;
			exit;
		else
			Delay_Time := Duration(0.5 * Ada.Numerics.Float_Random.Random(F_Gen));
			Value:= D_Rand.Random(D_Gen);
			delay Delay_Time;
			loop 
				buffer.Producer_Turn(Producer_Flag);
				exit when Producer_Flag;
			end loop;
			buffer.Insert(Value);
		end select;
		end loop;
	end producer;

	task body consumer is 
		Message: constant String := "consumer executing";
		Consumer_Flag: Boolean := False;
		Sum : Integer := 0;
        F_Gen: Ada.Numerics.Float_Random.Generator;
		Value : PInteger;
        Delay_Time : Duration;
	begin
		Ada.Numerics.Float_Random.Reset(F_Gen);

		Main_Cycle:
		loop 
		Delay_Time:= Duration(1.5 * Ada.Numerics.Float_Random.Random(F_Gen));
		delay Delay_Time;

		loop 
			buffer.Consumer_Turn(Consumer_Flag);
			exit when Consumer_Flag;
		end loop;

		loop
			buffer.Consumer_Turn(Consumer_Flag);
			buffer.Remove (Value);
			Sum := Sum + Value;

			--Put_Line ("Consumed: " & Integer'Image(Value));
			--Put_Line ("Sum: " & Integer'Image(Sum));

			if Sum > 100 then
				Put_Line ("Consumer: Reached termination condition. Sum = " & Integer'Image(Sum));
               	exit Main_Cycle;
            end if;
			exit when not Consumer_Flag;
        end loop;
      end loop Main_Cycle;
		
	  producer.stop;
	  buffer.stop;
    
		exception
			  when TASKING_ERROR =>
				  Put_Line("Buffer finished before producer");
		Put_Line("Ending the consumer");
	end consumer;
begin
	Put_Line(Message);
end comm1;
