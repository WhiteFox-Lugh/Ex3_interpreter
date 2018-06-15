# 実験4 interpreter

アライさんにインタプリタの実装おまかせなのだー！！

## 進捗
### 実装
- ML1
  * [x] Ex.3.2.1 [必修]
  * [x] Ex.3.2.2 [★★]
  * [ ] Ex.3.2.3 [★]
  * [x] Ex.3.2.4 [★★]
- ML2
  * [x] Ex.3.3.1 [必修]
  * [ ] Ex.3.3.2 [★★]
  * [ ] Ex.3.3.3 [★★]
  * [ ] Ex.3.3.4 [★★]
- ML3
  * [x] Ex.3.4.1 [必修]
  * [ ] Ex.3.4.2 [★★]
  * [ ] Ex.3.4.3 [★]
  * [x] Ex.3.4.4 [★]
  * [ ] Ex.3.4.5 [★]
  * [ ] Ex.3.4.6 [★]
- ML4
  * [x] Ex.3.5.1 [必修]
  * [ ] Ex.3.5.2 [★★]
- ML5
  * [ ] Ex.3.6.1 [★★]
  * [ ] Ex.3.6.2 [★]
  * [ ] Ex.3.6.3 [★]
  * [ ] Ex.3.6.4 [★★★]
  * [ ] Ex.3.6.5 [★★]
  * [ ] Ex.3.6.6 [(`･ω･´)]
  * [ ] Ex.3.6.7 [(`･ω･´)]

### レポート
- ML1
  * [ ] Ex.3.2.1 [必修]
  * [ ] Ex.3.2.2 [★★]
  * [ ] Ex.3.2.3 [★]
  * [ ] Ex.3.2.4 [★★]
- ML2
  * [ ] Ex.3.3.1 [必修]
  * [ ] Ex.3.3.2 [★★]
  * [ ] Ex.3.3.3 [★★]
  * [ ] Ex.3.3.4 [★★]
- ML3
  * [ ] Ex.3.4.1 [必修]
  * [ ] Ex.3.4.2 [★★]
  * [ ] Ex.3.4.3 [★]
  * [ ] Ex.3.4.4 [★]
  * [ ] Ex.3.4.5 [★]
  * [ ] Ex.3.4.6 [★]
- ML4
  * [ ] Ex.3.5.1 [必修]
  * [ ] Ex.3.5.2 [★★]
- ML5
  * [ ] Ex.3.6.1 [★★]
  * [ ] Ex.3.6.2 [★]
  * [ ] Ex.3.6.3 [★]
  * [ ] Ex.3.6.4 [★★★]
  * [ ] Ex.3.6.5 [★★]
  * [ ] Ex.3.6.6 [(`･ω･´)]
  * [ ] Ex.3.6.7 [(`･ω･´)]

## Memo
### Exercise 3.2.3 [★]
短絡評価ができていない。
`BinOp`で1つだけ引数を渡すものを作る？

### Exercise 3.4.4 [★]
プログラムの内容は以下の通り。

```
let makefact = fun maker -> fun x -> 
if x < 1 then 1
else x * maker maker (x + -1) in
let timesx = fun x -> makefact makefact x in
timesx 6;;
```