//############################################################################## ■

#include<Math.cl>
#include<Math.D4x4.cl>
#include<Color.cl>
#include<Raytracing.cl>
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
               const  image2d_t Tex,
               const  sampler_t Sam )
{
  THit Hit;
  TTap Tap;
  bool Emi;

  for ( int N = 0; N < 10; N++ )
  {
    Hit.Dis = INFINITY;   // 衝突点までの距離
    Hit.Pos = (float3)0;  // 衝突点の位置
    Hit.Nor = (float3)0;  // 衝突点の法線
    Hit.Mat = 0;          // 衝突点の材質ＩＤ

    ///// 物体

    if ( ObjPlane( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );  // 地面とレイの交差判定
    if ( ObjSpher( Ray, &Tap ) ) CheckHit( &Hit, &Tap, 1 );  // 球体とレイの交差判定

    ///// 材質

    switch( Hit.Mat )  // 材質の選択
    {
      case 0: Emi = MatSkyer( Ray, &Hit, Tex, Sam ); break;  // 空
      case 1: Emi = MatMirro( Ray, &Hit           ); break;  // 鏡面
    }

    if ( !Emi ) break;  // 放射しなければ終了

    Ray->Pos += FLOAT_EPS3 * sign( dot( Ray->Vec, Hit.Nor ) ) * Hit.Nor;  // 放射点をシフト
  }
}

//############################################################################## ■

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Render

kernel void Render( write_only image2d_t  Imager,
                    global     TSingleM4* Camera,
                    read_only  image2d_t  Textur,
                    const      sampler_t  Samplr )
{
  TPix Pix;
  TEye Eye;
  TScr Scr;
  TCam Cam;
  TRay Ray;

  Pix.Siz.x = get_global_size( 0 );  // 画像のピクセル数Ｘ
  Pix.Siz.y = get_global_size( 1 );  // 画像のピクセル数Ｙ

  Pix.Pos.x = get_global_id( 0 );  // ピクセルの整数座標Ｘ
  Pix.Pos.y = get_global_id( 1 );  // ピクセルの整数座標Ｙ

  Eye.Pos.x = 0;  // 視点位置Ｘ
  Eye.Pos.y = 0;  // 視点位置Ｙ
  Eye.Pos.z = 0;  // 視点位置Ｚ

  Scr.Siz.x = 4;  // スクリーンのサイズＸ
  Scr.Siz.y = 3;  // スクリーンのサイズＹ

  Scr.Pos.x = Scr.Siz.x * ( ( Pix.Pos.x + 0.5 ) / Pix.Siz.x - 0.5 );  // スクリーン上の標本位置Ｘ
  Scr.Pos.y = Scr.Siz.y * ( 0.5 - ( Pix.Pos.y + 0.5 ) / Pix.Siz.y );  // スクリーン上の標本位置Ｙ
  Scr.Pos.z = -2;                                                     // スクリーン上の標本位置Ｚ

  Cam.Mov = Camera[0];  // カメラの姿勢

  Ray.Pos   = MulPos( Cam.Mov, Eye.Pos );                         // レイの出射位置
  Ray.Vec   = MulVec( Cam.Mov, normalize( Scr.Pos - Eye.Pos ) );  // レイのベクトル
  Ray.Wei.x = 1;                                                  // レイのウェイトＲ
  Ray.Wei.y = 1;                                                  // レイのウェイトＧ
  Ray.Wei.z = 1;                                                  // レイのウェイトＢ
  Ray.Rad.x = 0;                                                  // レイの輝度Ｒ
  Ray.Rad.y = 0;                                                  // レイの輝度Ｇ
  Ray.Rad.z = 0;                                                  // レイの輝度Ｂ

  Raytrace( &Ray, Textur, Samplr );  // レイトレーシング

  Pix.Rad.xyz = Ray.Wei * Ray.Rad;  // ピクセル輝度

  Pix.Col = GammaCorrect( ToneMap( Pix.Rad, 100 ), 2.2 );  // ピクセル色

  write_imagef( Imager, Pix.Pos, (float4)( Pix.Col, 1 ) );  // ピクセル色を保存
}

//############################################################################## ■
