STARTDATE=2016-12-31
ENDDATE=2017-01-03

TEMPDATE=$STARTDATE
while [ 1 ] ; do
  # �����̏���
  echo $TEMPDATE

  # ENDDATE���܂ŏ���������I���
  if [ $TEMPDATE = $ENDDATE ] ; then
    break
  fi

  # ���t���C���N�������g
  TEMPDATE=`date -d "$TEMPDATE 1day" "+%Y-%m-%d"`
done