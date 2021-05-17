#ifndef RAYTRACE_MATERIAL_CL
#define RAYTRACE_MATERIAL_CL
//############################################################################## ■

#include<Math.cl>
#include<Raytrace.core.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatSkyere
// 空

bool MatSkydom( TRay*  const     Ray,
                const  THit*     Hit,
                uint4* const     See,
                const  image2d_t Tex,
                const  sampler_t Sam )
{
  Ray->Rad += read_imagef( Tex, Sam, VecToSky( Ray->Vec ) ).rgb;  // 輝度を加算

  return false;  // レイトレーシングの中断
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatMirror
// 鏡面

bool MatMirror( TRay*  const Ray,
                const  THit* Hit,
                uint4* const See )
{
  Ray->Pos = Hit->Pos + _EmiShift * Hit->Nor;  // 反射位置
  Ray->Vec = Reflect( Ray->Vec, Hit->Nor );    // 反射ベクトル

  return true;  // レイトレーシングの続行
}

//############################################################################## ■
#endif
