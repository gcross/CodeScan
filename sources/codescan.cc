//@+leo-ver=4-thin
//@+node:gcross.20081228030258.15:@thin codescan.cc
//@@language c

#include "codescan.hpp"

using namespace prepare;

#include <iostream>
using namespace std;

//@+others
//@+node:gcross.20081228030258.20:initialize_connection
connection* initialize_connection(const char* connection_information) {
    connection *conn = new connection(connection_information);
    //conn->trace(stderr);

    conn->prepare("insert_code","INSERT INTO codes (code_id,graph) VALUES ($1,$2)")
        ("uuid", treat_string)("varchar", treat_string);

    conn->prepare("insert_stabilizer","INSERT INTO stabilizers (code_id,operator_id) VALUES ($1,$2)")
        ("uuid", treat_string)("uuid", treat_string);

    conn->prepare("insert_operator","INSERT INTO operators (operator_id,vertex,label) VALUES ($1,$2::int4,$3::int4)")
        ("uuid", treat_string)("integer", treat_direct)("integer", treat_direct);

    conn->prepare("insert_qubit","INSERT INTO qubits (qubit_id,X_operator_id,Y_operator_id,Z_operator_id) VALUES ($1,$2,$3,$4)")
        ("uuid", treat_string)("uuid", treat_string)("uuid", treat_string)("uuid", treat_string);

    conn->prepare("insert_gauge","INSERT INTO gauge_qubits (code_id,qubit_id) VALUES ($1,$2)")
        ("uuid", treat_string)("uuid", treat_string);

    conn->prepare("insert_logical","INSERT INTO logical_qubits (code_id,qubit_id,mwe_operator_id) VALUES ($1,$2,$3)")
        ("uuid", treat_string)("uuid", treat_string)("uuid", treat_string);

    conn->prepare("insert_edge","INSERT INTO hamiltonians (code_id,edge,label1,label2) VALUES ($1,$2::int4,$3::int4,$4::int4)")
        ("uuid", treat_string)("integer", treat_direct)("integer", treat_direct)("integer", treat_direct);

//@+at
// Statements which update the progress counters.
//@-at
//@@c

    conn->prepare("update_counter","UPDATE graphs_being_scanned SET number_of_systems_considered=$2::int8, timestamp=now() WHERE graph=$1")
        ("varchar", treat_string)("integer", treat_direct);

    conn->prepare("create_counter","INSERT INTO graphs_being_scanned (graph) VALUES ($1)")
        ("varchar", treat_string);

    conn->prepare("find_completed_graph","SELECT graph FROM graphs_completed WHERE graph=$1")
        ("varchar", treat_string);

    conn->prepare("find_counter","SELECT number_of_systems_considered::int8 FROM graphs_being_scanned WHERE graph=$1")
        ("varchar", treat_string);

    conn->prepare("delete_counter","DELETE FROM graphs_being_scanned WHERE graph=$1")
        ("varchar", treat_string);

    conn->prepare("add_graph_to_completed","INSERT INTO graphs_completed (graph) VALUES ($1)")
        ("varchar", treat_string);

    return conn;
}
//@-node:gcross.20081228030258.20:initialize_connection
//@+node:gcross.20081228030258.21:finalize_connection
void finalize_connection(connection* conn) {
    delete conn;
}
//@-node:gcross.20081228030258.21:finalize_connection
//@+node:gcross.20081228030258.24:scan_?
#define make_scan(n) long long scan_##n(connection_base* conn, const char* graph, int number_of_permutations, int **permutations) { return scan<n>(conn,graph,number_of_permutations,permutations); }

make_scan(4);
make_scan(5);
make_scan(6);
make_scan(7);
make_scan(8);
make_scan(9);
make_scan(10);
make_scan(11);
make_scan(12);
make_scan(13);
make_scan(14);
make_scan(15);
make_scan(16);
make_scan(17);
make_scan(18);
make_scan(19);
make_scan(20);
//@-node:gcross.20081228030258.24:scan_?
//@-others

void test(char* s, int rows, int cols, int** numbers) {
    cout << "Graph is: " << s << endl;
    for(int i = 0; i < rows; i++) {
        for(int j = 0; j < cols; j++)
            cout << numbers[i][j] << " ";
        cout << endl;
    }
}
//@-node:gcross.20081228030258.15:@thin codescan.cc
//@-leo
