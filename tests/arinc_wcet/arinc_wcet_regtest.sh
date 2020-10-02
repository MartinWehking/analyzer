failure=0
res=$(./goblint --set arinc_cfg_id 0 --sets result fast_xml tests/arinc_wcet/extracted.json 2>&1)
if echo $res | grep -q -e 'deadline'; then
   echo "Test 0 failed, deadline violated"
   failure=1
fi
if echo $res | grep -q -e 'error'; then
   echo "Test 0 failed, error:"
   echo $res
   failure=1
fi

res=$(./goblint --set arinc_cfg_id 1 --sets result fast_xml tests/arinc_wcet/minimal_problematic.json 2>&1)
if echo $res | grep -q -e 'deadline'; then
   echo "Test 1 failed, deadline violated"
   failure=1
fi
if echo $res | grep -q -e 'error'; then
   echo "Test 1 failed, error:"
   echo $res
   failure=1
fi

exit $failure
