//############################################################################## ■

#include<Math.cl>
#include<Math.D4x4.cl>
#include<Color.cl>
#include<Raytrace.core.cl>
#include<Raytrace.Object.cl>
#include<Raytrace.Material.cl>

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
               const  sampler_t Sam )
{
  THit Hit;
  TTap Tap;
  bool Emi;

  for ( int N = 0; N < 10; N++ )
  {
    Hit.Dis = INFINITY;                                                         // 衝突点までの距離
    Hit.Pos = (float3)( 0 );                                                    // 衝突点の位置
    Hit.Nor = (float3)( 0 );                                                    // 衝突点の法線
    Hit.Mat = 0;                                                                // 衝突点の材質ＩＤ

    ///// 物体

    if ( ObjGround( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );                    // 地面とレイの交差判定
    if ( ObjSphere( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );                    // 球体とレイの交差判定

    ///// 材質

    switch( Hit.Mat )                                                           // 材質の選択
    {
      case 0: Emi = MatSkydom( Ray, &Hit, See, Tex, Sam ); break;               // 空
      case 1: Emi = MatMirror( Ray, &Hit, See           ); break;               // 鏡面
    }

    if ( !Emi ) break;                                                          // 放射しなければ終了
  }
}

//############################################################################## ■

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main

kernel void Main( write_only image2d_t  Imager,
                  read_write image2d_t  Seeder,
                  read_write image2d_t  Accumr,
                  global     uint*      AccumN,
                  global     TSingleM4* Camera,
                  read_only  image2d_t  Textur,
                  const      sampler_t  Samplr )
{
  TPix Pix;
  TEye Eye;
  TScr Scr;
  TCam Cam;
  TRay Ray;

  Pix.Siz = (int2)( get_global_size( 0 ), get_global_size( 1 ) );               // 画像サイズ
  Pix.Pos = (int2)( get_global_id  ( 0 ), get_global_id  ( 1 ) );               // ピクセル座標
  Pix.See = read_imageui( Seeder, Pix.Pos );                                    // 乱数シードを取得
  Pix.Rad = read_imagef ( Accumr, Pix.Pos ).xyz;                                // ピクセル輝度を取得
  Pix.Acu = AccumN[0];                                                          // 蓄積数を取得

  Eye.Pos = (float3)( 0, 0, 0 );                                                // 視点位置

  Scr.Pos.x = 4.0 * ( Pix.Pos.x + 0.5 ) / Pix.Siz.x - 2.0;                      // スクリーン上のピクセル位置
  Scr.Pos.y = 1.5 - 3.0 * ( Pix.Pos.y + 0.5 ) / Pix.Siz.y;
  Scr.Pos.z = -2;

  Cam.Mov = Camera[0];                                                          // カメラの姿勢

  for ( int N = Pix.Acu+1; N < Pix.Acu+1+100; N++ )
  {
    Ray.Pos = MulPos( Cam.Mov, Eye.Pos );                                       // レイ出射位置
    Ray.Vec = MulVec( Cam.Mov, normalize( Scr.Pos - Eye.Pos ) );                // レイベクトル
    Ray.Rad = (float3)( 0 );                                                    // レイ輝度

    Raytrace( &Ray, &Pix.See, Textur, Samplr );                                 // レイトレーシング

    Pix.Rad += ( Ray.Rad - Pix.Rad ) / N;                                       // ピクセル輝度
  }

  Pix.Col = GammaCorrect( ToneMap( Pix.Rad, 100 ), 2.2 );                       // ピクセル色

  write_imagef ( Accumr, Pix.Pos, (float4)( Pix.Rad, 1 ) );                     // ピクセル輝度を保存
  write_imageui( Seeder, Pix.Pos,           Pix.See      );                     // 乱数シードを保存
  write_imagef ( Imager, Pix.Pos, (float4)( Pix.Col, 1 ) );                     // ピクセル色を保存
}

//############################################################################## ■
