# RaytracerCL

[OpenCL](https://ja.wikipedia.org/wiki/OpenCL) を用いて、ＧＰＵ上でレイトレーシングする方法。

![](https://github.com/LUXOPHIA/RaytracerCL/raw/Lesson-01/--------/_SCREENSHOT/RaytracerCL.png)

## ■ Delphi 側

### ◆ Main.pas

#### ▼ TForm1
中核をなすクラスは以下の６つ（★）である。

```delphi
type
  TForm1 = class(TForm)
    ･･･
  public
    { public 宣言 }
    _Platfo :TCLPlatfo;
    _Device :TCLDevice;
    _Contex :TCLContex;
    _Queuer :TCLQueuer;
    _Imager :TCLImager2DxBGRAxUFix8; ★
    _ImaFMX :ICLStream2DxBGRAxUFix8_FMX; ★
    _Camera :TCLBuffer<TSingleM4>; ★
    _Textur :TCLImager2DxRGBAxSFlo32; ★
    _TexHDR :ICLStream2DxRGBAxSFlo32_HDR; ★
    _Samplr :TCLSamplr; ★
    _Execut :TCLExecut;
    _Buildr :TCLBuildr;
    _Kernel :TCLKernel;
    ･･･
  end;
```

* **`_Imager :TCLImager2DxBGRAxUFix8`**  
8bit 整数の RGBA 画像。レンダリングした画像を Delphi 側で受ける。
* **`_ImaFMX :ICLStream2DxBGRAxUFix8_FMX`**  
`_Imager`を`FMX.Graphics.TBitmap`として扱うための入出力クラス。
* **`_Camera :TCLBuffer<TSingleM4>`**  
カメラの座標変換行列。
* **`_Textur :TCLImager2DxRGBAxSFlo32`**  
32bit 実数のRGBA画像。空のテクスチャとして用いる 360° の HDRI を OpenCL 側へ渡す。
* **`_TexHDR :ICLStream2DxRGBAxSFlo32_HDR`**  
`_Textur`を`*.hdr`画像として扱うための入出力クラス。
* **`_Samplr :TCLSamplr`**  
`_Textur`の補間方法を指定するサンプラークラス。

#### ▼ TForm1.FormCreate
アプリ起動時に３つの初期化（★）を行う。
```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
     _MouseS := [];
     _MouseC := TPointF.Create( -60, +10 );
     MakeContext; ★
     MakeArgumes; ★
     MakeProgras; ★
     ShowBuild;
     TOpenCL.Show( MemoS.Lines );
end;
```

#### ▼ TForm1.MakeContext
OpenCL の初期化を行う。
```delphi
procedure TForm1.MakeContext;
begin
     _Platfo := TOpenCL.Platfos[ 0 ];  // プラットフォームの選択
     _Device := _Platfo.Devices[ 0 ];  // デバイスの選択
     _Contex := TCLContex.Create( _Platfo );  //コンテキストの生成
     _Queuer := _Contex.Queuers[ _Device ];  //コマンドキューの設定
end;
```
基本的なライブラリの使い方については、以下を参照して下さい。
* https://luxophia.github.io/OpenCL/ja/

#### ▼ TForm1.MakeArgumes
OpenCL とデータをやり取りするための各種クラスを生成し設定する。
```delphi
procedure TForm1.MakeArgumes;
begin
     _Imager := TCLImager2DxBGRAxUFix8.Create( _Contex, _Queuer );  //画像オブジェクトの生成
     _Imager.CountX := 800;  //横ピクセル数
     _Imager.CountY := 600;  //縦ピクセル数

     _ImaFMX := TCLStream2DxBGRAxUFix8_FMX.Create( _Imager );  // _Imager に対する入出力オプジェクトの生成

     _Camera := TCLBuffer<TSingleM4>.Create( _Contex, _Queuer );  //カメラの座標変換行列の配列オブジェクトの生成
     _Camera.Count := 1;  //要素数は１個

     _Textur := TCLImager2DxRGBAxSFlo32.Create( _Contex, _Queuer );  // 32bit RGBA 画像オプジェクトの生成

     _TexHDR := TCLStream2DxRGBAxSFlo32_HDR.Create( _Textur );  // _Textur に対する入出力オプジェクトの生成
     _TexHDR.LoadFromFile( '..\..\_DATA\Luxo-Jr_2000x1000.hdr' );  // HDRI のロード

     _Samplr := TCLSamplr.Create( _Contex );  // _Textur に対するサンプラーオプジェクトの生成 
end;
```

#### ▼ TForm1.MakeProgras
OpenCL のコードをロードしてコンパイルする。
```delphi
procedure TForm1.MakeProgras;
begin
     with _Contex.Librars do  // ライブラリ用のソースコードをロード
     begin
          Add.Source.LoadFromFile( '..\..\_DATA\Math.cl'              );  // ライブラリのプログラムオブジェクトを生成し、ソースコードをロード
          Add.Source.LoadFromFile( '..\..\_DATA\Math.D4x4.cl'         );
          Add.Source.LoadFromFile( '..\..\_DATA\Color.cl'             );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytracing.cl'        );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Object.cl'   );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Material.cl' );
     end;

     _Execut := TCLExecut.Create( _Contex );  // 実行用のプログラムオブジェクトを生成
     _Execut.Source.LoadFromFile( '..\..\_DATA\Raytracing.Render.cl' );  // 実行用のソースコードをロード

     _Buildr := _Execut.Buildrs[ _Device ];  // ビルダーオブジェクトを生成

     if not Assigned( _Buildr.Handle ) then Exit; { _Buildr is Error! }

     _Kernel := _Execut.Kernels.Add( 'Render', _Queuer );  // 実行用のソースコードの中から`Render`関数に対してカーネルオブジェクトを生成

     Assert( Assigned( _Kernel.Handle ), '_Kernel is Error!' );

     _Kernel.GloSizX := _Imager.CountX;  //レンダリング画像のピクセル数を設定
     _Kernel.GloSizY := _Imager.CountY;  //

     _Kernel.Parames['Imager'] := _Imager;  // `Render`関数の引数に各種オブジェクトを接続
     _Kernel.Parames['Camera'] := _Camera;  //
     _Kernel.Parames['Textur'] := _Textur;  //
     _Kernel.Parames['Samplr'] := _Samplr;  //

     Assert( _Kernel.Parames.FindsOK, '_Kernel.Parames.FindsOK is Error!' );
     Assert( _Kernel.Parames.BindsOK, '_Kernel.Parames.BindsOK is Error!' );

     Timer1.Enabled := True;  //タイマーオブジェクトを起動
end;
```
#### ▼ TForm1.Timer1Timer
タイマーオブジェクトを用いて、レンダリングを定期実行する。
```delphi
procedure TForm1.Timer1Timer(Sender: TObject);
begin
     _Camera.Data.Map;  // 配列オブジェクトをＣＰＵ側のメモリ空間へマップ
     _Camera.Data[ 0 ] := TSingleM4.RotateY( DegToRad( -_MouseC.X ) )
                        * TSingleM4.RotateX( DegToRad( -_MouseC.Y ) )
                        * TSingleM4.Translate( 0, 0, 3 );  // カメラの座標変換行列の書き換え
     _Camera.Data.Unmap; // 配列オブジェクトをアンマップ

     _Kernel.Run;  // カーネルの実行

     _ImaFMX.CopyTo( Image1.Bitmap );  // _Imager に出力されたレンダリング画像を Image1 コンポーネントへ転写
end;
```

----

* **Delphi IDE** @ Embarcadero  
https://www.embarcadero.com/jp/products/delphi/starter
