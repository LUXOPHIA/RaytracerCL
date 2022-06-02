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

bool MatSkyer( TRay*  const     Ray,
               const  THit*     Hit,
               uint4* const     See,
               const  image2d_t Tex,
               const  sampler_t Sam )
{
  Ray->Rad += read_imagef( Tex, Sam, VecToSky( Ray->Vec ) ).xyz;  // 輝度を加算

  return false;  // レイトレーシングの中断
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatMirror
// 鏡面

bool MatMirro( TRay*  const Ray,
               const  THit* Hit,
               uint4* const See )
{
  Ray->Pos = Hit->Pos;                       // 反射位置
  Ray->Vec = Reflect( Ray->Vec, Hit->Nor );  // 反射ベクトル

  return true;  // レイトレーシングの続行
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MatWater
// 水面

bool MatWater( TRay*  const Ray,
               const  THit* Hit,
               uint4* const See )
{
  float  IOR0, IOR1, F;
  float3 Nor;

  if( dot( Ray->Vec, Hit->Nor ) < 0 )
  {
    IOR0 = 1.000;
    IOR1 = 1.333;
    Nor  = +Hit->Nor;
  }
  else
  {
    IOR0 = 1.333;
    IOR1 = 1.000;
    Nor  = -Hit->Nor;
  }

  F = Fresnel( Ray->Vec, Nor, IOR0, IOR1 );

  Ray->Pos = Hit->Pos;

  if ( Rand( See ) < F ) Ray->Vec = Reflect( Ray->Vec, Nor             );  // 反射
                    else Ray->Vec = Refract( Ray->Vec, Nor, IOR0, IOR1 );  // 屈折

  return true;
}

//############################################################################## ■
#endif
