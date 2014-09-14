ハッシュタグで検索されたGeoタグつきのツイートを抽出するためのすくりぷと

## 事前準備

スクリプトファイルを実行する場所に.envというファイルを用意し、以下の環境変数を設定しておく
```
TWITTER_OAUTH_API_KEY=
TWITTER_OAUTH_API_SECRET=
TWITTER_OAUTH_ACCESS_TOKEN=
TWITTER_OAUTH_ACCESS_TOKEN_SECRET=
```

## 使い方

```
  $ scrap-tweet-locations -q ハッシュタグ名
```

## ビルド方法

```
  $ cabal install
  $ cabal build
```

## TODO

  1. Tableau Publicとの統合
  2. 抽出できる数が少ないので多くすること
  3. やっつけコードのリファクタリング

