#ifndef RAYTRACING_MATERIAL_CL
#define RAYTRACING_MATERIAL_CL
//############################################################################## ■

#include<Math.cl>
#include<Raytracing.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatSkyere
// 空

bool MatSkyer( TRay* const     Ray,
               const THit*     Hit,
               const image2d_t Tex,
               const sampler_t Sam )
{
  Ray->Rad += read_imagef( Tex, Sam, VecToSky( Ray->Vec ) ).xyz;  // 輝度を加算

  return false;  // レイトレーシングの中断
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatMirror
// 鏡面

bool MatMirro( TRay* const Ray,
               const THit* Hit )
{
  Ray->Pos = Hit->Pos;                       // 反射位置
  Ray->Vec = Reflect( Ray->Vec, Hit->Nor );  // 反射ベクトル

  return true;  // レイトレーシングの続行
}

//############################################################################## ■
#endif
