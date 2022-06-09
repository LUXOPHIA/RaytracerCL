#ifndef MATH_D3X3_CL
#define MATH_D3X3_CL
//############################################################################## ■

#include<Math.D2x2.cl>

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleM3
// 3 × 3 行列

typedef struct
{
  float _11, _21, _31,
        _12, _22, _32,
        _13, _23, _33;
}
TSingleM3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleM3

///////////////////////////////////////////////////////////////////////// 演算子

TSingleM3 __attribute__((overloadable)) Mul( const TSingleM3 A, const TSingleM3 B )
{
  TSingleM3 Result;

  Result._11 = A._11 * B._11 + A._12 * B._21 + A._13 * B._31;
  Result._12 = A._11 * B._12 + A._12 * B._22 + A._13 * B._32;
  Result._13 = A._11 * B._13 + A._12 * B._23 + A._13 * B._33;

  Result._21 = A._21 * B._11 + A._22 * B._21 + A._23 * B._31;
  Result._22 = A._21 * B._12 + A._22 * B._22 + A._23 * B._32;
  Result._23 = A._21 * B._13 + A._22 * B._23 + A._23 * B._33;

  Result._31 = A._31 * B._11 + A._32 * B._21 + A._33 * B._31;
  Result._32 = A._31 * B._12 + A._32 * B._22 + A._33 * B._32;
  Result._33 = A._31 * B._13 + A._32 * B._23 + A._33 * B._33;

  return Result;
}

float3 __attribute__((overloadable)) Mul( const TSingleM3 A, const float3 B )
{
  float3 Result;

  Result.x = A._11 * B.x + A._12 * B.y + A._13 * B.z;
  Result.y = A._21 * B.x + A._22 * B.y + A._23 * B.z;
  Result.z = A._31 * B.x + A._32 * B.y + A._33 * B.z;

  return Result;
}

//------------------------------------------------------------------------------

TSingleM3 __attribute__((overloadable)) Div( const TSingleM3 A, const float B )
{
  TSingleM3 Result;

  Result._11 = A._11 / B;  Result._12 = A._12 / B;  Result._13 = A._13 / B;
  Result._21 = A._21 / B;  Result._22 = A._22 / B;  Result._23 = A._23 / B;
  Result._31 = A._31 / B;  Result._32 = A._32 / B;  Result._33 = A._33 / B;

  return Result;
}

//------------------------------------------------------------------------------

TSingleM3 __attribute__((overloadable)) Transpose( const TSingleM3 M )
{
  TSingleM3 Result;

  Result._11 = M._11;  Result._12 = M._21;  Result._13 = M._31;
  Result._21 = M._12;  Result._22 = M._22;  Result._23 = M._32;
  Result._31 = M._13;  Result._32 = M._23;  Result._33 = M._33;

  return Result;
}

//------------------------------------------------------------------------------

float __attribute__((overloadable)) Det( const TSingleM3 M )
{
  return M._11 * ( M._22 * M._33 - M._23 * M._32 )
       + M._12 * ( M._23 * M._31 - M._21 * M._33 )
       + M._13 * ( M._21 * M._32 - M._22 * M._31 );
}

//------------------------------------------------------------------------------

TSingleM3 __attribute__((overloadable)) Adjugate( const TSingleM3 M )
{
  TSingleM3 Result;

  Result._11 = -Det( (TSingleM2){ M._22, M._23,
                                  M._32, M._33 } );

  Result._21 = -Det( (TSingleM2){ M._21, M._23,
                                  M._31, M._33 } );

  Result._31 = +Det( (TSingleM2){ M._21, M._22,
                                  M._31, M._32 } );

  Result._12 = -Det( (TSingleM2){ M._12, M._13,
                                  M._32, M._33 } );

  Result._22 = +Det( (TSingleM2){ M._11, M._13,
                                  M._31, M._33 } );

  Result._32 = -Det( (TSingleM2){ M._11, M._12,
                                  M._31, M._32 } );

  Result._13 = +Det( (TSingleM2){ M._12, M._13,
                                  M._22, M._23 } );

  Result._23 = -Det( (TSingleM2){ M._11, M._13,
                                  M._21, M._23 } );

  Result._33 = +Det( (TSingleM2){ M._11, M._12,
                                  M._21, M._22 } );

  return Result;
}

//------------------------------------------------------------------------------

TSingleM3 __attribute__((overloadable)) Inverse( const TSingleM3 M )
{
  TSingleM3 A = Adjugate( M );

  return Div( A, M._11 * A._11
               + M._12 * A._21
               + M._13 * A._31 );
}

//############################################################################## ■
#endif