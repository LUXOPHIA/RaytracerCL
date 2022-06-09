//############################################################################## ■

#include<Math.cl>
#include<Math.D4x4.cl>
#include<Color.cl>
#include<Raytracing.cl>
#include<Raytracing.Object.cl>
#include<Raytracing.Material.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckHit

void CheckHit( THit* const Hit,
               const TTap* Tap,
               const int   Mat )
{
  if ( Tap->Dis < Hit->Dis )  // 物体毎の衝突距離 < 現在の最短衝突距離
  {
    Hit->Dis = Tap->Dis;
    Hit->Pos = Tap->Pos;
    Hit->Nor = Tap->Nor;
    Hit->Mat =      Mat;
  }
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Raytrace

void Raytrace( TRay*  const     Ray,
               uint4* const     See,
               const  image2d_t Tex,
               const  sampler_t Sam,
               global TShaper*  Shapers )
{
  THit Hit;
  TTap Tap;
  bool Emi;

  for ( int N = 0; N < 5; N++ )
  {
    Hit.Dis = INFINITY;   // 衝突点までの距離
    Hit.Pos = (float3)0;  // 衝突点の位置
    Hit.Nor = (float3)0;  // 衝突点の法線
    Hit.Mat = 0;          // 衝突点の材質ＩＤ

    ///// 物体

    //if ( ObjPlane( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 3 );  // 地面とレイの交差判定

    for ( int i = 0; i < 100; i++ )
    {
      if ( ObjSpher( Ray, &Tap, Shapers[ i ].Mov ) ) CheckHit( &Hit, &Tap, 1 );  // 球体とレイの交差判定
    }

    ///// 材質

    switch( Hit.Mat )  // 材質の選択
    {
      case 0: Emi = MatSkyer( Ray, &Hit, See, Tex, Sam ); break;  // 空
      case 1: Emi = MatMirro( Ray, &Hit, See           ); break;  // 鏡面
      case 2: Emi = MatWater( Ray, &Hit, See           ); break;  // 水面
      case 3: Emi = MatDiffu( Ray, &Hit, See           ); break;  // 地面
    }

    if ( !Emi ) break;  // 放射しなければ終了

    Ray->Pos += FLOAT_EPS3 * sign( dot( Ray->Vec, Hit.Nor ) ) * Hit.Nor;  // 放射点をシフト
  }
}

//############################################################################## ■

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Render

kernel void Render( write_only image2d_t  Imager ,
                    read_write image2d_t  Seeder ,
                    read_write image2d_t  Accumr ,
                    global     TSingleM4* Camera ,
                    read_only  image2d_t  Textur ,
                    const      sampler_t  Samplr ,
                    global     TShaper*   Shapers )
{
  TPix Pix;
  TEye Eye;
  TScr Scr;
  TCam Cam;
  TRay Ray;

  Pix.Siz = (int2)( get_global_size( 0 ), get_global_size( 1 ) );  // 画像のピクセル数
  Pix.Pos = (int2)( get_global_id  ( 0 ), get_global_id  ( 1 ) );  // ピクセルの整数座標
  Pix.See = read_imageui( Seeder, Pix.Pos );                       // 乱数シードを取得
  Pix.Rad = read_imagef ( Accumr, Pix.Pos );                       // ピクセル輝度を取得

  Eye.Pos = (float3)( RandCirc(&Pix.See) * 0.05f, 0 );  // 視点位置

  float2 A = (float2)( Rand(&Pix.See)+Rand(&Pix.See)+Rand(&Pix.See)+Rand(&Pix.See)-2,
                       Rand(&Pix.See)+Rand(&Pix.See)+Rand(&Pix.See)+Rand(&Pix.See)-2 );

  Scr.Siz   = (float2)( 4, 3 );                                             // スクリーンのサイズ
  Scr.Pos.x = Scr.Siz.x * ( ( Pix.Pos.x + 0.5 + A.x ) / Pix.Siz.x - 0.5 );  // スクリーン上の標本位置
  Scr.Pos.y = Scr.Siz.y * ( 0.5 - ( Pix.Pos.y + 0.5 + A.y ) / Pix.Siz.y );
  Scr.Pos.z = -2;

  Cam.Mov = Camera[0];  // カメラの姿勢

  for ( int N = 1; N <= 4; N++ )
  {
    Ray.Pos = MulPos( Cam.Mov, Eye.Pos );                         // レイの出射位置
    Ray.Vec = MulVec( Cam.Mov, normalize( Scr.Pos - Eye.Pos ) );  // レイのベクトル
    Ray.Wei = (float3)1;                                          // レイのウェイト
    Ray.Rad = (float3)0;                                          // レイの輝度

    Raytrace( &Ray, &Pix.See, Textur, Samplr, Shapers );  // レイトレーシング

    Pix.Rad.w   += 1;                                                // 標本数
    Pix.Rad.xyz += ( Ray.Wei * Ray.Rad - Pix.Rad.xyz ) / Pix.Rad.w;  // ピクセル輝度
  }

  Pix.Col = GammaCorrect( ToneMap( Pix.Rad.xyz, 100 ), 2.2 );  // ピクセル色

  write_imagef ( Accumr, Pix.Pos,           Pix.Rad      );  // ピクセル輝度を保存
  write_imageui( Seeder, Pix.Pos,           Pix.See      );  // 乱数シードを保存
  write_imagef ( Imager, Pix.Pos, (float4)( Pix.Col, 1 ) );  // ピクセル色を保存
}

//############################################################################## ■
