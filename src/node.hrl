-record(node,
        {node_name=void,node_pid=void,port_pid=void,node_id=void,
         monitor_pids=void,
         gc_pid=void,
         node_node,
         options,
	 cookie=void,
	 enter_classes=[],
         symbolic_name=void,
         unix_pid=void,ping_retry=5000,connect_timeout=1000,
         max_java_start_tries=3,call_timeout,num_start_tries=0}).

