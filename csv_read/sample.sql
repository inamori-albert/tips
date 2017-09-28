select
  *
from
  csvread(
      -- CSVファイルパス
      'C:\Users\toru_inamori\Documents\csv_read\sample.csv',
      null,
      -- 文字コード
      'UTF-8',
      -- 区切り文字
      char(9) -- カンマ区切りなら『','』,tab区切りなら『char(9)』で指定
  )
;
