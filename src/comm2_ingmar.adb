with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;

use Ada.Calendar;
use Ada.Text_IO;

procedure comm2_ingmar is
    subtype Produced_Integer is Integer range 0..20;
    package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Produced_Integer);

    use Integer_Vectors;
    use type Ada.Containers.Count_Type;

    package Random_Discrete is new Ada.Numerics.Discrete_Random(Produced_Integer);

    Message: constant String := "Process communication";

    protected Buffer is
        procedure Can_Push(Value: out Boolean);
        procedure Can_Pop(Value: out Boolean);
        entry Push(Value: in Produced_Integer);
        entry Pop(Value: out Produced_Integer);
        function Is_Stopped return Boolean;
        procedure Stop;
    private
        Internal: Integer_Vectors.Vector;
        Stopped: Boolean := False;
    end Buffer;

    protected body Buffer is
        procedure Can_Push(Value: out Boolean) is
        begin
            Value := Internal.Length < 10;
        end Can_Push;

        procedure Can_Pop(Value: out Boolean) is
        begin
            Value := Internal.Length > 0;
        end Can_Pop;

        entry Push(Value: in Produced_Integer)
            when Internal.Length < 10 and not Stopped is
        begin
            Internal.Append(Value);
        end Push;

        entry Pop(Value: out Produced_Integer)
            when Internal.Length > 0 and not Stopped is
        begin
            Value := Internal.First_Element;
            Internal.Delete_First;
        end Pop;

        function Is_Stopped return Boolean is
        begin
            return Stopped;
        end Is_Stopped;

        procedure Stop is
        begin
            Stopped := True;
        end Stop;
    end Buffer;

    task producer is
        entry Start;
        entry Stop;
    end producer;

    task consumer is
        -- Nothing Needed, consumer is controlling the other tasks
    end consumer;

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
                    Buffer.Can_Push(Can_Push);
                    exit when Can_Push;
                end loop;
                Buffer.Push(Random_Int);
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
                Buffer.Can_Pop(Can_Pop);
                exit when Can_Pop or Buffer.Is_Stopped;
                delay 0.1;
            end loop;

            exit Main_Loop when Buffer.Is_Stopped;

            loop
                Buffer.Can_Pop(Can_Pop);
                exit when not Can_Pop;

                Buffer.Pop(Popped);
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
        Buffer.Stop;

    exception
        when TASKING_ERROR => Put_Line("Buffer finished before producer");
        Put_Line("Ending the consumer");
    end consumer;
begin
    Put_Line(Message);
end comm2_ingmar;
