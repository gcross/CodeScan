//@+leo-ver=4-thin
//@+node:gcross.20090107230224.4:@thin codescan_tests.cc
//@@language c++

#include <cstdarg>
#include "codescan.hpp"
#include "unit--.h"

//@+others
//@+node:gcross.20090108164821.42:Utility Functions
//@+node:gcross.20090108164821.43:iv
template<int nbits, int nitems> typename Vertex<nbits>::index_vector iv(int first, ...) {
    va_list vl;
    va_start(vl,first);
    assert(nitems <= nbits);
    typename Vertex<nbits>::index_vector vector;
    vector.push_back(first);
    for(int i=1; i<nitems; ++i)
        vector.push_back(va_arg(vl,int));
    va_end(vl);
    return vector;
}
//@-node:gcross.20090108164821.43:iv
//@-node:gcross.20090108164821.42:Utility Functions
//@+node:gcross.20090108164821.44:Tests
testSuite(compare_vertices_tests);

//@+others
//@+node:gcross.20090108164821.47:TwoVertexTests
subSuite(TwoVertexTests,compare_vertices_tests);
//@nonl
//@+node:gcross.20090108164821.45:_2_SameLabel
testCase(_2_SameLabel, TwoVertexTests)
{
    Vertex<2>::index_vector
    v1_labels           (iv<2,1>(1)),
    v1_adjacent_vertices(iv<2,1>(1)),
    v2_labels           (iv<2,1>(1));
    int v2_vertex_to_edge_map[2] = {0,-1};
    int permutation[2]           = {1,0};

    Ordering ordering = TerminalVertex<2>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.45:_2_SameLabel
//@+node:gcross.20090108164821.46:_2_DifferentLabels_1
testCase(_2_DifferentLabels_1, TwoVertexTests)
{
    Vertex<2>::index_vector
    v1_labels           (iv<2,1>(1)),
    v1_adjacent_vertices(iv<2,1>(1)),
    v2_labels           (iv<2,1>(2));
    int v2_vertex_to_edge_map[2] = {0,-1};
    int permutation[2]           = {1,0};

    Ordering ordering = TerminalVertex<2>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.46:_2_DifferentLabels_1
//@+node:gcross.20090108164821.48:_2_DifferentLabels_2
testCase(_2_DifferentLabels_2, TwoVertexTests)
{
    Vertex<2>::index_vector
    v1_labels           (iv<2,1>(1)),
    v1_adjacent_vertices(iv<2,1>(1)),
    v2_labels           (iv<2,1>(3));
    int v2_vertex_to_edge_map[2] = {0,-1};
    int permutation[2]           = {1,0};

    Ordering ordering = TerminalVertex<2>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.48:_2_DifferentLabels_2
//@-node:gcross.20090108164821.47:TwoVertexTests
//@+node:gcross.20090108164821.49:ThreeVertexTests
subSuite(ThreeVertexTests,compare_vertices_tests);
//@nonl
//@+node:gcross.20090108164821.50:_3_SameLabels_1
testCase(_3_SameLabels_1, ThreeVertexTests)
{
    const Vertex<3>::index_vector
    v1_labels           (iv<3,2>(1,2)),
    v1_adjacent_vertices(iv<3,2>(1,2)),
    v2_labels           (iv<3,2>(1,2));
    const int v2_vertex_to_edge_map[3] = {0,-1,1};
    const int permutation[3]           = {1,0,2};

    Ordering ordering = TerminalVertex<3>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.50:_3_SameLabels_1
//@+node:gcross.20090108164821.57:_3_SameLabels_2
testCase(_3_SameLabels_2, ThreeVertexTests)
{
    const Vertex<3>::index_vector
    v1_labels           (iv<3,2>(1,2)),
    v1_adjacent_vertices(iv<3,2>(1,2)),
    v2_labels           (iv<3,2>(2,1));
    const int v2_vertex_to_edge_map[3] = {0,-1,1};
    const int permutation[3]           = {1,0,2};

    Ordering ordering = TerminalVertex<3>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.57:_3_SameLabels_2
//@+node:gcross.20090108164821.55:_3_DifferentLabels_1
testCase(_3_DifferentLabels_1, ThreeVertexTests)
{
    const Vertex<3>::index_vector
    v1_labels           (iv<3,2>(1,2)),
    v1_adjacent_vertices(iv<3,2>(1,2)),
    v2_labels           (iv<3,2>(1,1));
    const int v2_vertex_to_edge_map[3] = {0,-1,1};
    const int permutation[3]           = {1,0,2};

    Ordering ordering = TerminalVertex<3>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,GT);
};
//@-node:gcross.20090108164821.55:_3_DifferentLabels_1
//@+node:gcross.20090108164821.56:_3_DifferentLabels_2
testCase(_3_DifferentLabels_2, ThreeVertexTests)
{
    const Vertex<3>::index_vector
    v1_labels           (iv<3,2>(1,2)),
    v1_adjacent_vertices(iv<3,2>(1,2)),
    v2_labels           (iv<3,2>(2,2));
    const int v2_vertex_to_edge_map[3] = {0,-1,1};
    const int permutation[3]           = {1,0,2};

    Ordering ordering = TerminalVertex<3>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,GT);
};
//@-node:gcross.20090108164821.56:_3_DifferentLabels_2
//@+node:gcross.20090108164821.58:_3_DifferentLabels_3
testCase(_3_DifferentLabels_3, ThreeVertexTests)
{
    const Vertex<3>::index_vector
    v1_labels           (iv<3,2>(1,1)),
    v1_adjacent_vertices(iv<3,2>(1,2)),
    v2_labels           (iv<3,2>(1,2));
    const int v2_vertex_to_edge_map[3] = {0,-1,1};
    const int permutation[3]           = {1,0,2};

    Ordering ordering = TerminalVertex<3>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,LT);
};
//@-node:gcross.20090108164821.58:_3_DifferentLabels_3
//@-node:gcross.20090108164821.49:ThreeVertexTests
//@+node:gcross.20090108164821.59:FourVertexTests
subSuite(FourVertexTests,compare_vertices_tests);
//@nonl
//@+node:gcross.20090108164821.60:_4_DifferentAdjacents_SameLabels_1
testCase(_4_DifferentAdjacents_SameLabels_1, FourVertexTests)
{
    const Vertex<4>::index_vector
    v1_labels           (iv<4,2>(1,2)),
    v1_adjacent_vertices(iv<4,2>(1,2)),
    v2_labels           (iv<4,2>(1,2));
    const int v2_vertex_to_edge_map[4] = {0,-1,-1,1};
    const int permutation[4]           = {1,0,3,2};

    Ordering ordering = TerminalVertex<4>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.60:_4_DifferentAdjacents_SameLabels_1
//@+node:gcross.20090108164821.61:_4_DifferentAdjacents_SameLabels_2
testCase(_4_DifferentAdjacents_SameLabels_2, FourVertexTests)
{
    const Vertex<4>::index_vector
    v1_labels           (iv<4,2>(1,2)),
    v1_adjacent_vertices(iv<4,2>(1,2)),
    v2_labels           (iv<4,2>(1,2));
    const int v2_vertex_to_edge_map[4] = {1,-1,-1,0};
    const int permutation[4]           = {1,0,3,2};

    Ordering ordering = TerminalVertex<4>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.61:_4_DifferentAdjacents_SameLabels_2
//@+node:gcross.20090108164821.64:_4_SameAdjacents_SameLabels_1
testCase(_4_SameAdjacents_SameLabels_1, FourVertexTests)
{
    const Vertex<4>::index_vector
    v1_labels           (iv<4,2>(1,2)),
    v1_adjacent_vertices(iv<4,2>(1,2)),
    v2_labels           (iv<4,2>(1,2));
    const int v2_vertex_to_edge_map[4] = {-1,0,1,-1};
    const int permutation[4]           = {0,1,2,3};

    Ordering ordering = TerminalVertex<4>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.64:_4_SameAdjacents_SameLabels_1
//@+node:gcross.20090108164821.65:_4_SameAdjacents_SameLabels_2
testCase(_4_SameAdjacents_SameLabels_2, FourVertexTests)
{
    const Vertex<4>::index_vector
    v1_labels           (iv<4,2>(1,2)),
    v1_adjacent_vertices(iv<4,2>(1,2)),
    v2_labels           (iv<4,2>(1,2));
    const int v2_vertex_to_edge_map[4] = {-1,0,1,-1};
    const int permutation[4]           = {3,1,2,0};

    Ordering ordering = TerminalVertex<4>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.65:_4_SameAdjacents_SameLabels_2
//@+node:gcross.20090108164821.66:_4_SameAdjacents_SameLabels_3
testCase(_4_SameAdjacents_SameLabels_3, FourVertexTests)
{
    const Vertex<4>::index_vector
    v1_labels           (iv<4,2>(1,2)),
    v1_adjacent_vertices(iv<4,2>(1,2)),
    v2_labels           (iv<4,2>(1,2));
    const int v2_vertex_to_edge_map[4] = {-1,0,1,-1};
    const int permutation[4]           = {0,2,1,3};

    Ordering ordering = TerminalVertex<4>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.66:_4_SameAdjacents_SameLabels_3
//@-node:gcross.20090108164821.59:FourVertexTests
//@+node:gcross.20090108164821.67:FiveVertexTests
subSuite(FiveVertexTests,compare_vertices_tests);
//@nonl
//@+node:gcross.20090108164821.68:_5_SameAdjacents_SameLabels_1
testCase(_5_SameAdjacents_SameLabels_1, FiveVertexTests)
{
    const Vertex<5>::index_vector
    v1_labels           (iv<5,3>(1,1,2)),
    v1_adjacent_vertices(iv<5,3>(2,3,4)),
    v2_labels           (iv<5,3>(1,1,2));
    const int v2_vertex_to_edge_map[5] = {-1,-1,0,1,2};
    const int permutation[5]           = {0,1,2,3,4};

    Ordering ordering = TerminalVertex<5>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.68:_5_SameAdjacents_SameLabels_1
//@+node:gcross.20090108164821.69:_5_SameAdjacents_SameLabels_2
testCase(_5_SameAdjacents_SameLabels_2, FiveVertexTests)
{
    const Vertex<5>::index_vector
    v1_labels           (iv<5,3>(1,1,2)),
    v1_adjacent_vertices(iv<5,3>(2,3,4)),
    v2_labels           (iv<5,3>(1,1,2));
    const int v2_vertex_to_edge_map[5] = {-1,-1,0,1,2};
    const int permutation[5]           = {1,0,2,3,4};

    Ordering ordering = TerminalVertex<5>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.69:_5_SameAdjacents_SameLabels_2
//@+node:gcross.20090108164821.70:_5_SameAdjacents_SameLabels_3
testCase(_5_SameAdjacents_SameLabels_3, FiveVertexTests)
{
    const Vertex<5>::index_vector
    v1_labels           (iv<5,3>(1,1,2)),
    v1_adjacent_vertices(iv<5,3>(2,3,4)),
    v2_labels           (iv<5,3>(1,1,2));
    const int v2_vertex_to_edge_map[5] = {-1,-1,0,1,2};
    const int permutation[5]           = {0,1,3,2,4};

    Ordering ordering = TerminalVertex<5>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.70:_5_SameAdjacents_SameLabels_3
//@+node:gcross.20090108164821.71:_5_SameAdjacents_SwappedLabels_1
testCase(_5_SameAdjacents_SwappedLabels_1, FiveVertexTests)
{
    const Vertex<5>::index_vector
    v1_labels           (iv<5,3>(1,1,2)),
    v1_adjacent_vertices(iv<5,3>(2,3,4)),
    v2_labels           (iv<5,3>(1,1,2));
    const int v2_vertex_to_edge_map[5] = {-1,-1,0,1,2};
    const int permutation[5]           = {0,1,2,4,3};

    Ordering ordering = TerminalVertex<5>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,LT);
};
//@-node:gcross.20090108164821.71:_5_SameAdjacents_SwappedLabels_1
//@+node:gcross.20090108164821.72:_5_SameAdjacents_SwappedLabels_2
testCase(_5_SameAdjacents_SwappedLabels_2, FiveVertexTests)
{
    const Vertex<5>::index_vector
    v1_labels           (iv<5,3>(1,1,2)),
    v1_adjacent_vertices(iv<5,3>(2,3,4)),
    v2_labels           (iv<5,3>(1,1,2));
    const int v2_vertex_to_edge_map[5] = {-1,-1,0,1,2};
    const int permutation[5]           = {0,1,2,4,3};

    Ordering ordering = TerminalVertex<5>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,LT);
};
//@-node:gcross.20090108164821.72:_5_SameAdjacents_SwappedLabels_2
//@-node:gcross.20090108164821.67:FiveVertexTests
//@+node:gcross.20090108164821.62:NineVertexTests
subSuite(NineVertexTests,compare_vertices_tests);
//@nonl
//@+node:gcross.20090108164821.63:Nine_1
testCase(Nine_1, NineVertexTests)
{
    const Vertex<9>::index_vector
    v1_labels           (iv<9,4>(1,2,1,2)),
    v1_adjacent_vertices(iv<9,4>(0,1,4,5)),
    v2_labels           (iv<9,4>(1,2,2,1));
    const int v2_vertex_to_edge_map[9] = {-1,-1,0,1,2,3,-1,-1};
    const int permutation[9]           = {2,3,1,0,5,4,6,8,7};

    Ordering ordering = TerminalVertex<9>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,EQ);
};
//@-node:gcross.20090108164821.63:Nine_1
//@+node:gcross.20090108164821.73:Nine_2
testCase(Nine_2, NineVertexTests)
{
    const Vertex<9>::index_vector
    v1_labels           (iv<9,4>(1,2,2,1)),
    v1_adjacent_vertices(iv<9,4>(2,3,4,5)),
    v2_labels           (iv<9,4>(1,2,1,2));
    const int v2_vertex_to_edge_map[9] = {0,1,-1,-1,2,3,-1,-1};
    const int permutation[9]           = {2,3,1,0,5,4,6,8,7};

    Ordering ordering = TerminalVertex<9>::compare_vertices(
        v1_labels,
        v1_adjacent_vertices,
        v2_labels,
        v2_vertex_to_edge_map,
        permutation
    );
    assertEqual(ordering,GT);
};
//@-node:gcross.20090108164821.73:Nine_2
//@-node:gcross.20090108164821.62:NineVertexTests
//@-others
//@-node:gcross.20090108164821.44:Tests
//@-others

useDefaultMain;
//@-node:gcross.20090107230224.4:@thin codescan_tests.cc
//@-leo
