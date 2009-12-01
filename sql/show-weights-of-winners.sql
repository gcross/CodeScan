select codes.graph as graph, codes.code_id as code_id, count(*) as weight
from codes
natural join logical_qubits
inner join operators on mwe_operator_id=operator_id
group by codes.graph, codes.code_id, mwe_operator_id;
