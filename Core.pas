unit Core;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.D4, LUX.D4x4;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TParticle

     TParticle = record
       Mov :TSingleM4;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TPoinPCD

     TPoinPCD = record
       RGB :UInt32;
       Pos :TSingle3D;
       _1  :Byte;
       _2  :Byte;
       _3  :Byte;
       _4  :Byte;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {CLASS}

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

procedure LoadPCD( const FileName_:String );

implementation //############################################################### ■

uses System.SysUtils,
     Main;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {RECORD}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {CLASS}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

procedure LoadPCD( const FileName_:String );
var
   F :TFileReader;
   L :String;
   Ws :TArray<String>;
   PsN, I :Integer;
   Ps :TArray<TPoinPCD>;
   A :TParticle;
begin
     F := TFileReader.Create( FileName_, TEncoding.ANSI );

     Assert( F.ReadLine = '# .PCD v0.7 - Point Cloud Data file format' );
     Assert( F.ReadLine = 'VERSION 0.7' );
     Assert( F.ReadLine = 'FIELDS rgb x y z _' );
     Assert( F.ReadLine = 'SIZE 4 4 4 4 1' );
     Assert( F.ReadLine = 'TYPE F F F F U' );
     Assert( F.ReadLine = 'COUNT 1 1 1 1 4' );
     Assert( F.ReadLine = 'WIDTH 39' );
     Assert( F.ReadLine = 'HEIGHT 1' );
     Assert( F.ReadLine = 'VIEWPOINT 0 0 0 1 0 0 0' );

     L := F.ReadLine;
     Ws := L.Split( [' '] );
     Assert( Ws[0] = 'POINTS' );
     PsN := Ws[1].ToInteger;

     Assert( F.ReadLine = 'DATA binary' );

     SetLength( Ps, PsN );

     F.Read( Ps[0], SizeOf( TPoinPCD ) * PsN );

     Form1._Particles.Count := PsN;
     Form1._Particles.Data.Map;

     for I := 0 to PsN-1 do
     begin
          with Ps[ I ].Pos do A.Mov := TSingleM4.RotateX( -P2i )
                                     * TSingleM4.Translate( 5*X, 5*Y, 5*Z )
                                     * TSingleM4.Translate( 0.75, -0.3, 1.5 )
                                     * TSingleM4.Scale( 0.05, 0.05, 0.05 );

          Form1._Particles.Data[ I ] :=  A;
     end;

     Form1._Particles.Data.Unmap;

     F.Free;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■