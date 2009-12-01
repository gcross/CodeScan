//@+leo-ver=4-thin
//@+node:gcross.20081202171210.2:@thin codescan.hpp
//@@language c

#include <uuid++.hh>

#include <iostream>
#include <iomanip>
#include "codelib.hpp"

#include <pqxx/pqxx>

using namespace pqxx;
using namespace std;

//@+others
//@+node:gcross.20081220122752.10:Enumerations
enum Ordering { LT, EQ, GT };
//@-node:gcross.20081220122752.10:Enumerations
//@+node:gcross.20081202171210.3:Classes
//@+node:gcross.20081203145321.3:Biterator class
template<int nbits> class Biterator {

    string descriptor;

    int charnum;
    unsigned char mask;

public:

    Biterator(string descriptor_) :
        descriptor(descriptor_),
        charnum(1),
        mask(32)
    {
        assert(descriptor[0]=='A'+nbits-2);
        int nedges = nbits*(nbits-1)/2;
        int nchars = nedges / 6 + 1;
        if(nedges%6>0) ++nchars;
        assert(descriptor.length() == nchars);
    }

    bool operator*() { return (descriptor[charnum]-63)&mask; }

    void operator++() {
        mask >>= 1;
        if(mask == 0) {
            mask = 32;
            ++charnum;
        }
    }

};
//@-node:gcross.20081203145321.3:Biterator class
//@+node:gcross.20081203145321.2:Vertex Classes
//@+node:gcross.20081202171210.4:Vertex
template<int nbits> struct Vertex {

    //@    @+others
    //@+node:gcross.20081202171210.5:(types)
    typedef static_quantum_operator<nbits> quantum_operator_type;
    typedef static_vector<quantum_operator_type,nbits*(nbits-1)> operator_vector;
    typedef typename operator_vector::iterator operator_iterator;
    typedef static_vector<operator_iterator,nbits-1> operator_iterator_vector;
    typedef typename operator_iterator_vector::iterator operator_iterator_vector_iterator;

    typedef static_vector<size_t,nbits> index_vector;
    typedef typename index_vector::iterator index_vector_iterator;

    typedef qubit<quantum_operator_type> qubit_type;
    typedef static_vector<qubit_type,nbits*nbits> qubit_vector;

    typedef qec<
        quantum_operator_type,
        qubit_vector,
        operator_vector,
        index_vector
    > qec_type;

    enum depthtype { First, Second, Rest };
    //@-node:gcross.20081202171210.5:(types)
    //@+node:gcross.20081202171210.6:(fields)
    operator_iterator_vector adjacent_edges;
    index_vector adjacent_vertices, current_labels;
    int vertex_to_edge_map[nbits];
    Vertex* previous_vertex;

    int myindex;
    //@-node:gcross.20081202171210.6:(fields)
    //@+node:gcross.20081202171210.11:(constructor)
    Vertex(int myindex_) : myindex(myindex_), previous_vertex(NULL) { }
    //@-node:gcross.20081202171210.11:(constructor)
    //@+node:gcross.20081202171210.14:(destructor)
    virtual ~Vertex() { if(previous_vertex != NULL) delete previous_vertex; }
    //@-node:gcross.20081202171210.14:(destructor)
    //@+node:gcross.20081202171210.7:do_next
    virtual void do_next() = 0;
    //@-node:gcross.20081202171210.7:do_next
    //@+node:gcross.20081202171210.8:recurse/3
    void recurse(
        depthtype depth,
        operator_iterator_vector_iterator current_edge,
        index_vector_iterator current_index
    ) {
        if(current_edge == adjacent_edges.end()) {
            if (depth == Rest) do_next();
        } else {
            switch(depth) {
                case First:
                    set(current_edge,current_index,1);
                    recurse(Second,current_edge+1,current_index+1);
                    break;
                case Second:
                    set(current_edge,current_index,1);
                    recurse(Second,current_edge+1,current_index+1);
                    set(current_edge,current_index,2);
                    recurse(Rest,current_edge+1,current_index+1);
                    break;
                case Rest:
                    for(int i = 1; i <= 3; i++) {
                        set(current_edge,current_index,i);
                        recurse(Rest,current_edge+1,current_index+1);
                    }
                    break;
            }
        }
    }
    //@-node:gcross.20081202171210.8:recurse/3
    //@+node:gcross.20081202171210.9:recurse/0
    void recurse() { recurse(First,adjacent_edges.begin(),current_labels.begin()); }

    //@-node:gcross.20081202171210.9:recurse/0
    //@+node:gcross.20081202171210.13:set
    void set(
        operator_iterator_vector_iterator current_edge,
        index_vector_iterator current_index,
        unsigned char new_value
    ) {
        (*current_edge)->set(myindex,new_value);
        *current_index = new_value;
    }
    //@-node:gcross.20081202171210.13:set
    //@+node:gcross.20081202171210.15:start
    void start() {
        if(previous_vertex==NULL)
            recurse();
        else
            previous_vertex->start();
    }
    //@-node:gcross.20081202171210.15:start
    //@-others

};
//@-node:gcross.20081202171210.4:Vertex
//@+node:gcross.20081202171210.10:NonterminalVertex
template<int nbits> struct NonterminalVertex : public Vertex<nbits> {

    //@    @+others
    //@+node:gcross.20081220122752.3:(fields)
    Vertex<nbits>* next_vertex;
    //@nonl
    //@-node:gcross.20081220122752.3:(fields)
    //@+node:gcross.20081220122752.4:(constructor)
    NonterminalVertex(Vertex<nbits>* next_vertex_) :
        Vertex<nbits>(next_vertex_->myindex+1),
        next_vertex(next_vertex_)
    { next_vertex->previous_vertex = this; }
    //@-node:gcross.20081220122752.4:(constructor)
    //@+node:gcross.20081220122752.5:do_next
    virtual void do_next() {
        next_vertex->recurse();
    }
    //@-node:gcross.20081220122752.5:do_next
    //@-others

};
//@-node:gcross.20081202171210.10:NonterminalVertex
//@+node:gcross.20081202171210.12:TerminalVertex
template<int nbits> struct TerminalVertex : public Vertex<nbits> {

    //@    @+others
    //@+node:gcross.20081220122752.6:(types)
    typedef typename Vertex<nbits>::operator_vector operator_vector;
    typedef typename Vertex<nbits>::quantum_operator_type quantum_operator_type;
    typedef typename Vertex<nbits>::qec_type qec_type;
    typedef typename Vertex<nbits>::index_vector index_vector;
    typedef typename Vertex<nbits>::qubit_type qubit_type;
    typedef typename Vertex<nbits>::qubit_vector qubit_vector;
    //@nonl
    //@-node:gcross.20081220122752.6:(types)
    //@+node:gcross.20081220122752.7:(fields)
    unsigned long long number_of_systems_examined, number_of_systems_considered, starting_system_number;

    const static int nops = nbits*(nbits-1);

    Vertex<nbits>* vertices[nbits];

    operator_vector operators;

    int **permutations;
    int number_of_permutations;

    string graph_string;

    connection_base& conn;

    std::vector<pair<typename index_vector::iterator,typename index_vector::iterator> > edges;
    //@-node:gcross.20081220122752.7:(fields)
    //@+node:gcross.20081220122752.8:(constructor)
    TerminalVertex(connection_base& conn_, long long starting_system_number_=0) : Vertex<nbits>(0), conn(conn_), number_of_systems_examined(0), number_of_systems_considered(0), number_of_permutations(0), permutations(NULL), starting_system_number(starting_system_number_) {
        Vertex<nbits>** vertex_ptr = vertices+nbits-1;
        *vertex_ptr = this;
        for(--vertex_ptr; vertex_ptr >= vertices; --vertex_ptr) {
            *vertex_ptr = new NonterminalVertex<nbits>(*(vertex_ptr+1));
        }
    }
    //@-node:gcross.20081220122752.8:(constructor)
    //@+node:gcross.20081204012123.11:do_next
    virtual void do_next() {
        ++number_of_systems_considered;
        if(number_of_systems_considered < starting_system_number) return; //cout << "skipping..." << endl; return; }

        //@    << Determine whether we have seen an automorphim of this system before. >>
        //@+node:gcross.20081220122752.11:<< Determine whether we have seen an automorphim of this system before. >>
        for(int **permutation_ptr = permutations; permutation_ptr < permutations+number_of_permutations; ++permutation_ptr) {
            int *permutation = *permutation_ptr;
            for(int j = 0; j < nbits; ++j) {
                switch(compare_vertices(j,permutation[j],permutation)) {
                    case LT:
                        goto next_permutation;    // We've ruled out this permutation, so move on to the next one.
                    case GT:
                        return;   // We've seen this system before, so skip it.
                    case EQ:
                        break; // Need to keep looking at this permutation.
                }
            }
            next_permutation:  ;
        }
        //@-node:gcross.20081220122752.11:<< Determine whether we have seen an automorphim of this system before. >>
        //@nl

        //@    << Compute the code for this system. >>
        //@+node:gcross.20081220122752.12:<< Compute the code for this system. >>
        ++number_of_systems_examined;
        qec_type code(operators);
        //@nonl
        //@-node:gcross.20081220122752.12:<< Compute the code for this system. >>
        //@nl

        //@    << Check to see whether it is interesting, and if so record it. >>
        //@+node:gcross.20081220122752.13:<< Check to see whether it is interesting, and if so record it. >>
        index_vector minimum_weights;
        operator_vector minimum_weight_operators;
        code.compute_weights(minimum_weights,minimum_weight_operators,false);
        for(int i = 0; i < minimum_weights.size(); i++) {
            if(minimum_weights[i]>=3) {
                //@        << Record the code. >>
                //@+node:gcross.20081222162732.4:<< Record the code. >>
                // cout << code << endl;

                uuid code_id;
                code_id.make(UUID_MAKE_V4);
                char* code_id_string = code_id.string();
                const char* graph_c_string = graph_string.c_str();

                work T(conn);

                T.prepared("insert_code")((const char*)code_id_string)((const char*)graph_c_string).exec();

                for(int i = 0; i < code.stabilizers.size(); ++i) {
                    char* operator_id_string = insert_operator(T,code.stabilizers[i]);
                    T.prepared("insert_stabilizer")((const char*)code_id_string)((const char*)operator_id_string).exec();
                    free(operator_id_string);
                }

                for(int i = 0; i < code.gauge_qubits.size(); ++i) {
                    char* qubit_id_string = insert_qubit(T,code.gauge_qubits[i]);
                    T.prepared("insert_gauge")((const char*)code_id_string)((const char*)qubit_id_string).exec();
                    free(qubit_id_string);
                }

                for(int i = 0; i < code.logical_qubits.size(); ++i) {
                    char* qubit_id_string = insert_qubit(T,code.logical_qubits[i]);
                    char* mwe_operator_id_string = insert_operator(T,minimum_weight_operators[i]);
                    T.prepared("insert_logical")((const char*)code_id_string)((const char*)qubit_id_string)((const char*)mwe_operator_id_string).exec();
                    free(qubit_id_string);
                    free(mwe_operator_id_string);
                }

                for(int i = 0; i < edges.size(); ++i) {
                    T.prepared("insert_edge")((const char*)code_id_string)(i)(*edges[i].first)(*edges[i].second).exec();
                }

                T.prepared("update_counter")(graph_c_string)(number_of_systems_considered).exec();

                T.commit();

                cout << "Found code " << code_id_string << endl;

                free(code_id_string);

                //@-node:gcross.20081222162732.4:<< Record the code. >>
                //@nl
                break;
            }
        }
        //@-node:gcross.20081220122752.13:<< Check to see whether it is interesting, and if so record it. >>
        //@nl

        if(number_of_systems_considered % 10000000 == 0) {
            //@        << Update the number of systems we've considered. >>
            //@+node:gcross.20081228030258.17:<< Update the number of systems we've considered. >>
            work T(conn);
            const char* graph_c_string = graph_string.c_str();
            T.prepared("update_counter")(graph_c_string)(number_of_systems_considered).exec();
            T.commit();
            //@-node:gcross.20081228030258.17:<< Update the number of systems we've considered. >>
            //@nl
        }

    }
    //@-node:gcross.20081204012123.11:do_next
    //@+node:gcross.20081204012123.12:from_graph6
    void from_graph6(string s) {
        operators.clear();
        edges.clear();
        Biterator<nbits> b(s);
        for(int i = 0; i < nbits; ++i) {
            vertices[i]->adjacent_edges.clear();
            vertices[i]->adjacent_vertices.clear();
            vertices[i]->current_labels.clear();
            for(int j = 0; j < nbits; ++j)
                vertices[i]->vertex_to_edge_map[j] = -1;
        }
        for(int col = 1; col < nbits; ++col)
            for(int row = 0; row < col; ++row, ++b) {
                if(*b) {                
                    assert(vertices[row]->vertex_to_edge_map[col]==-1);
                    vertices[row]->vertex_to_edge_map[col] = vertices[row]->current_labels.size();
                    vertices[row]->current_labels.push_back(operators.size());
                    vertices[row]->adjacent_vertices.push_back(col);

                    assert(vertices[col]->vertex_to_edge_map[row]==-1);
                    vertices[col]->vertex_to_edge_map[row] = vertices[col]->current_labels.size();
                    vertices[col]->current_labels.push_back(operators.size());
                    vertices[col]->adjacent_vertices.push_back(row); 

                    edges.push_back(make_pair(vertices[row]->current_labels.end()-1,
                                              vertices[col]->current_labels.end()-1));

                    operators.push_back(quantum_operator_type(nbits));
                }
            }
        for(int i = 0; i < nbits; ++i) {
            for(typename index_vector::iterator indexptr = vertices[i]->current_labels.begin();
                indexptr != vertices[i]->current_labels.end();
                ++indexptr
            ) {
                vertices[i]->adjacent_edges.push_back(operators.begin()+*indexptr);
                *indexptr = 0;
            }
        }
        graph_string = s;
    }
    //@-node:gcross.20081204012123.12:from_graph6
    //@+node:gcross.20081220122752.2:compare_vertices
    Ordering inline compare_vertices(const int v1index, const int v2index, const int permutation[nbits]) {
        const Vertex<nbits> &v1 = *vertices[v1index],
                            &v2 = *vertices[v2index];
        return compare_vertices(
            v1.current_labels,
            v1.adjacent_vertices,
            v2.current_labels,
            v2.vertex_to_edge_map,
            permutation
        );
    }

    static Ordering compare_vertices(
        const index_vector& v1_labels,
        const index_vector& v1_adjacent_vertices,
        const index_vector& v2_labels,
        const int v2_vertex_to_edge_map[nbits],
        const int permutation[nbits]
    ) {
        size_t label_map_2[3] = {0,0,0}, next_label_2 = 1;
        assert(v1_labels.size()==v2_labels.size());
        for(int i = 0; i < v1_labels.size(); ++i) {
            const size_t label1 = v1_labels[i];
            const size_t index2 = v2_vertex_to_edge_map[permutation[v1_adjacent_vertices[i]]];
            assert(index2 != -1);
            size_t& label2 = label_map_2[v2_labels[index2]-1];
            if(label2==0) label2 = next_label_2++;
            if(label1 < label2)
                return LT;
            else if(label1 > label2)
                return GT;
        }
        return EQ;
    }
    //@-node:gcross.20081220122752.2:compare_vertices
    //@+node:gcross.20081222162732.7:insert_operator
    static char* insert_operator(transaction_base& T, const quantum_operator_type& op) {
        uuid operator_id;
        operator_id.make(UUID_MAKE_V4);
        char* operator_id_string = operator_id.string();
        for(int i = 0; i < op.length(); i++) {
            int pauli = op[i];
            if(pauli>0)
                T.prepared("insert_operator")
                    ((const char*)operator_id_string)(i)(pauli).exec();
        }
        return operator_id_string;
    }
    //@-node:gcross.20081222162732.7:insert_operator
    //@+node:gcross.20081222162732.9:insert_qubit
    static char* insert_qubit(transaction_base& T, const qubit_type& qubit) {
        char* X_operator_id_string = insert_operator(T,qubit.X);
        char* Y_operator_id_string = insert_operator(T,qubit.Y);
        char* Z_operator_id_string = insert_operator(T,qubit.Z);

        uuid qubit_id;
        qubit_id.make(UUID_MAKE_V4);
        char* qubit_id_string = qubit_id.string();

        T.prepared("insert_qubit")
            ((const char*)qubit_id_string)((const char*)X_operator_id_string)((const char*)Y_operator_id_string)((const char*)Z_operator_id_string).exec();

        free(X_operator_id_string);
        free(Y_operator_id_string);
        free(Z_operator_id_string);

        return qubit_id_string;
    }
    //@-node:gcross.20081222162732.9:insert_qubit
    //@-others


};
//@-node:gcross.20081202171210.12:TerminalVertex
//@-node:gcross.20081203145321.2:Vertex Classes
//@-node:gcross.20081202171210.3:Classes
//@+node:gcross.20081228030258.22:Functions
//@+node:gcross.20081228030258.23:scan
#define ALREADY_SCANNED -1

template<int N> long long scan(connection_base* conn, const char* graph, const int number_of_permutations, int **permutations) {
    long long starting_system_number = 0;
    {
        work T(*conn);
        if(T.prepared("find_completed_graph")(graph).exec().size() > 0) {
            cout << "Skipping " << graph << endl;
            return ALREADY_SCANNED;
        }

        result res = T.prepared("find_counter")(graph).exec();
        if(res.size() == 0) {
            T.prepared("create_counter")(graph).exec();
        } else {
            res.front()[0].to(starting_system_number);
        }
        T.commit();
    }

    cout << "Scanning " << graph << " starting at " << starting_system_number << endl;
    cout << "\t(There are " << number_of_permutations << " non-trivial automorphisms of this graph.)" << endl;

    TerminalVertex<N> v(*conn,starting_system_number);
    v.from_graph6(graph);
    v.number_of_permutations = number_of_permutations;
    v.permutations = permutations;
    v.start();

    {
        work T(*conn);
        T.prepared("add_graph_to_completed")(graph).exec();
        T.prepared("delete_counter")(graph).exec();
        T.commit();
    }

    cout << "...finished scanning through " << v.number_of_systems_considered << " systems, of which " << v.number_of_systems_examined << " were examined." << endl;

    return v.number_of_systems_examined;
}
//@-node:gcross.20081228030258.23:scan
//@-node:gcross.20081228030258.22:Functions
//@+node:gcross.20081228030258.18:External Functions
extern "C" {
    connection* initialize_connection(const char* connection_information);
    void finalize_connection(connection* conn);
    #define make_scan_definition(n) long long scan_##n(connection_base* conn, const char* graph, int number_of_permutations, int **permutations);
    make_scan_definition(4);
    make_scan_definition(5);
    make_scan_definition(6);
    make_scan_definition(7);
    make_scan_definition(8);
    make_scan_definition(9);
    make_scan_definition(10);
    make_scan_definition(11);
    make_scan_definition(12);
    make_scan_definition(13);
    make_scan_definition(14);
    make_scan_definition(15);
    make_scan_definition(16);
    make_scan_definition(17);
    make_scan_definition(18);
    make_scan_definition(19);
    make_scan_definition(20);
}

//@-node:gcross.20081228030258.18:External Functions
//@-others

//@-node:gcross.20081202171210.2:@thin codescan.hpp
//@-leo
