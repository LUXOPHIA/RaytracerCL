//############################################################################## ■

#include<Math.cl>
#include<Math.Matrix.cl>
#include<Color.cl>
#include<Raytrace.core.cl>
#include<Raytrace.Object.cl>
#include<Raytrace.Shader.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckHit

void CheckHit( THit* const Hit,
               const TTap* Tap,
               const int   Sha )
{
  if ( Tap->Dis < Hit->Dis )  // 物体毎の衝突距離 < 現在の最短衝突距離
  {
    Hit->Dis = Tap->Dis;
    Hit->Pos = Tap->Pos;
    Hit->Nor = Tap->Nor;
    Hit->Sha =      Sha;
  }
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Raytrace

void Raytrace( TRay* const     Ray,
               const image2d_t Tex,
               const sampler_t Sam )
{
  TTap Tap;
  bool Nex;

  for ( int N = 0; N < 10; N++ )
  {
    THit Hit = { INFINITY, (float3)( 0 ), (float3)( 0 ), 0 };                   // 衝突点の初期化

    ///// OBJECT

    if ( ObjGround( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );                    // 地面とレイの交差判定
    if ( ObjSphere( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );                    // 球体とレイの交差判定

    ///// SHADER

    switch( Hit.Sha )                                                           // シェーダーの切り替え
    {
      case 0: Nex = ShaSkydom( Ray, &Hit, Tex, Sam ); break;                    // 空
      case 1: Nex = ShaMirror( Ray, &Hit           ); break;                    // 鏡面
    }

    if ( !Nex ) break;                                                          // 反射しなければ終了
  }
}

//############################################################################## ■

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main

kernel void Main( global     TSingleM4* Buffer,
                  write_only image2d_t  Imager,
                  read_only  image2d_t  Textur,
                  read_only  sampler_t  Samplr )
{
  TPix Pix;
  TEye Eye;
  TScr Scr;
  TCam Cam;
  TRay Ray;

  Pix.Pos = (int2)( get_global_id  ( 0 ), get_global_id  ( 1 ) );               // ピクセルの座標
  Pix.Siz = (int2)( get_global_size( 0 ), get_global_size( 1 ) );               // ピクセルの総数

  Eye.Pos = (float3)( 0, 0, 0 );                                                // 視点の位置

  Scr.Pos.x = 4.0 * ( Pix.Pos.x + 0.5 ) / Pix.Siz.x - 2.0;                      // スクリーン上のピクセル位置
  Scr.Pos.y = 1.5 - 3.0 * ( Pix.Pos.y + 0.5 ) / Pix.Siz.y;
  Scr.Pos.z = -2;

  Cam.Mat = Buffer[0];                                                          // カメラの姿勢

  Ray.Pos = MulPos( Cam.Mat, Eye.Pos );                                         // レイの出射位置
  Ray.Vec = MulVec( Cam.Mat, normalize( Scr.Pos - Eye.Pos ) );                  // レイのベクトル
  Ray.Wei = (float3)( 1 );                                                      // レイのウェイト
  Ray.Emi = (float3)( 0 );                                                      // レイの輝度

  Raytrace( &Ray, Textur, Samplr );                                             // レイトレーシング

  Pix.Rad = Ray.Wei * Ray.Emi;                                                  // ピクセルの輝度
  Pix.Col = GammaCorrect( ToneMap( Pix.Rad, 100 ), 2.2 );                       // ピクセルの色

  write_imagef( Imager, Pix.Pos, (float4)( Pix.Col, 1 ) );                      // 画像へ書き出し
}

//############################################################################## ■
