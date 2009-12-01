<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet ekr_test?>
<leo_file>
<leo_header file_format="2" tnodes="0" max_tnode_index="0" clone_windows="0"/>
<globals body_outline_ratio="0.5">
	<global_window_position top="25" left="646" height="1168" width="1270"/>
	<global_log_window_position top="0" left="0" height="0" width="0"/>
</globals>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="gcross.20090222000027.1" str_leo_pos="0"><vh>@chapters</vh></v>
<v t="gcross.20091130193227.1557"><vh>@thin CodeScan.cabal</vh></v>
<v t="gcross.20091130193227.1575"><vh>@thin Setup.hs</vh></v>
<v t="gcross.20081230235952.9"><vh>Modules</vh>
<v t="gcross.20081230235952.10"><vh>@path sources/tests</vh>
<v t="gcross.20081211214849.2"><vh>@thin PermutationsTests.hs</vh></v>
<v t="gcross.20081221143112.4"><vh>@thin CodeGraphTests.hs</vh></v>
<v t="gcross.20090224112926.9"><vh>@thin EquivalenceTests.hs</vh></v>
</v>
<v t="gcross.20090401142814.2"><vh>@path sources/CodeQuest/CodeScan</vh>
<v t="gcross.20090224112926.24"><vh>@thin MyNauty.hs</vh></v>
<v t="gcross.20081225173032.3"><vh>@thin Code.hs</vh></v>
<v t="gcross.20081211125211.2"><vh>@thin Permutations.hs</vh></v>
<v t="gcross.20081221143112.2"><vh>@thin CodeGraph.hs</vh></v>
<v t="gcross.20090101195553.4"><vh>@thin Database.hs</vh></v>
<v t="gcross.20081228030258.25"><vh>@thin Scanner.hs</vh></v>
<v t="gcross.20090224112926.5"><vh>@thin Equivalence.hs</vh></v>
</v>
</v>
<v t="gcross.20090401142814.3"><vh>@path sources</vh>
<v t="gcross.20081230235952.6"><vh>Scanning</vh>
<v t="gcross.20081220122752.13" a="M"><vh>&lt;&lt; Check to see whether it is interesting, and if so record it. &gt;&gt;</vh>
<v t="gcross.20081222162732.4"><vh>&lt;&lt; Record the code. &gt;&gt;</vh></v>
</v>
<v t="gcross.20090107230224.5"><vh>@path tests</vh>
<v t="gcross.20090107230224.4"><vh>@thin codescan_tests.cc</vh></v>
</v>
<v t="gcross.20081202171210.2"
marks="gcross.20081220122752.13,"><vh>@thin codescan.hpp</vh></v>
<v t="gcross.20081228030258.15"><vh>@thin codescan.cc</vh></v>
<v t="gcross.20090107230224.2"><vh>@thin ScanCodes.hs</vh></v>
</v>
<v t="gcross.20081230235952.8"><vh>Utilities</vh>
<v t="gcross.20090331172821.2"><vh>Graph</vh>
<v t="gcross.20081230235952.3"><vh>@thin combg.hs</vh></v>
<v t="gcross.20081230235952.5"><vh>@thin MakeCondor.hs</vh></v>
<v t="gcross.20090223094425.2"><vh>@thin MakeBunchedCondor.hs</vh></v>
<v t="gcross.20081228030258.31"><vh>@thin CountCombinations.hs</vh></v>
<v t="gcross.20090101195553.5"><vh>@thin ChooseRandomGraphs.hs</vh></v>
<v t="gcross.20090223094425.3"><vh>@thin NeighborCountHistogram.hs</vh></v>
<v t="gcross.20090223094425.4"><vh>@thin NeighborCountStatistics.hs</vh></v>
<v t="gcross.20090224112926.3"><vh>@thin CanonicalForm.hs</vh></v>
</v>
<v t="gcross.20090331172821.3"><vh>Database</vh>
<v t="gcross.20090101195553.2"><vh>@thin FilterCompletedGraphs.hs</vh></v>
<v t="gcross.20090101195553.6"><vh>@thin ListWinningGraphs.hs</vh></v>
<v t="gcross.20090310192332.2"><vh>@thin ListWinningCodes.hs</vh></v>
<v t="gcross.20090224112926.2"><vh>@thin ListCompletedGraphs.hs</vh></v>
<v t="gcross.20090402153014.6"><vh>@thin CanonicalizeGraphs.hs</vh></v>
</v>
</v>
<v t="gcross.20081230235952.7"><vh>Viewing</vh>
<v t="gcross.20081222162732.12"><vh>@thin ShowCode.hs</vh></v>
<v t="gcross.20090402153014.4"><vh>@thin CanonicalizeCode.hs</vh></v>
</v>
<v t="gcross.20090224112926.8"><vh>Equivalence Testing</vh>
<v t="gcross.20090304103044.25"><vh>@thin ListEquivalentIdentifiers.hs</vh></v>
<v t="gcross.20090310145314.2"><vh>@thin ComputeEquivalenceClass.hs</vh></v>
<v t="gcross.20090311120257.2"><vh>@thin ComputeIdentifier.hs</vh></v>
</v>
</v>
</vnodes>
<tnodes>
<t tx="gcross.20081220122752.13">index_vector minimum_weights;
operator_vector minimum_weight_operators;
code.compute_weights(minimum_weights,minimum_weight_operators,false);
for(int i = 0; i &lt; minimum_weights.size(); i++) {
    if(minimum_weights[i]&gt;=3) {
        &lt;&lt; Record the code. &gt;&gt;
        break;
    }
}
</t>
<t tx="gcross.20081222162732.4">// cout &lt;&lt; code &lt;&lt; endl;

uuid code_id;
code_id.make(UUID_MAKE_V4);
char* code_id_string = code_id.string();
const char* graph_c_string = graph_string.c_str();

work T(conn);

T.prepared("insert_code")((const char*)code_id_string)((const char*)graph_c_string).exec();

for(int i = 0; i &lt; code.stabilizers.size(); ++i) {
    char* operator_id_string = insert_operator(T,code.stabilizers[i]);
    T.prepared("insert_stabilizer")((const char*)code_id_string)((const char*)operator_id_string).exec();
    free(operator_id_string);
}

for(int i = 0; i &lt; code.gauge_qubits.size(); ++i) {
    char* qubit_id_string = insert_qubit(T,code.gauge_qubits[i]);
    T.prepared("insert_gauge")((const char*)code_id_string)((const char*)qubit_id_string).exec();
    free(qubit_id_string);
}

for(int i = 0; i &lt; code.logical_qubits.size(); ++i) {
    char* qubit_id_string = insert_qubit(T,code.logical_qubits[i]);
    char* mwe_operator_id_string = insert_operator(T,minimum_weight_operators[i]);
    T.prepared("insert_logical")((const char*)code_id_string)((const char*)qubit_id_string)((const char*)mwe_operator_id_string).exec();
    free(qubit_id_string);
    free(mwe_operator_id_string);
}

for(int i = 0; i &lt; edges.size(); ++i) {
    T.prepared("insert_edge")((const char*)code_id_string)(i)(*edges[i].first)(*edges[i].second).exec();
}

T.prepared("update_counter")(graph_c_string)(number_of_systems_considered).exec();

T.commit();

cout &lt;&lt; "Found code " &lt;&lt; code_id_string &lt;&lt; endl;

free(code_id_string);

</t>
<t tx="gcross.20081230235952.6"></t>
<t tx="gcross.20081230235952.7"></t>
<t tx="gcross.20081230235952.8"></t>
<t tx="gcross.20081230235952.9"></t>
<t tx="gcross.20081230235952.10"></t>
<t tx="gcross.20090107230224.5"></t>
<t tx="gcross.20090222000027.1" str_leo_pos="0"></t>
<t tx="gcross.20090224112926.8"></t>
<t tx="gcross.20090331172821.2"></t>
<t tx="gcross.20090331172821.3"></t>
<t tx="gcross.20090401142814.2"></t>
<t tx="gcross.20090401142814.3"></t>
</tnodes>
</leo_file>
