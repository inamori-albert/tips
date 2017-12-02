STARTDATE=2016-12-31
ENDDATE=2017-01-03

TEMPDATE=$STARTDATE
while [ 1 ] ; do
  # 何かの処理
  echo $TEMPDATE

  # ENDDATE分まで処理したら終わり
  if [ $TEMPDATE = $ENDDATE ] ; then
    break
  fi

  # 日付をインクリメント
  TEMPDATE=`date -d "$TEMPDATE 1day" "+%Y-%m-%d"`
done