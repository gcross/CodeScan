--
-- PostgreSQL database dump
--

-- Started on 2009-01-01 13:56:41 EST

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1490 (class 1259 OID 16447)
-- Dependencies: 1761 3
-- Name: codes; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE codes (
    code_id uuid NOT NULL,
    graph text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.codes OWNER TO gcross;

--
-- TOC entry 1487 (class 1259 OID 16417)
-- Dependencies: 3
-- Name: encoded_qubits; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE encoded_qubits (
    code_id uuid NOT NULL,
    qubit_id uuid NOT NULL
);


ALTER TABLE public.encoded_qubits OWNER TO gcross;

--
-- TOC entry 1489 (class 1259 OID 16442)
-- Dependencies: 3 1487
-- Name: gauge_qubits; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE gauge_qubits (
)
INHERITS (encoded_qubits);


ALTER TABLE public.gauge_qubits OWNER TO gcross;

--
-- TOC entry 1493 (class 1259 OID 24584)
-- Dependencies: 1765 1766 3
-- Name: graphs_being_scanned; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE graphs_being_scanned (
    graph text NOT NULL,
    number_of_systems_considered integer DEFAULT 0 NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.graphs_being_scanned OWNER TO gcross;

--
-- TOC entry 1492 (class 1259 OID 24576)
-- Dependencies: 1764 3
-- Name: graphs_completed; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE graphs_completed (
    graph text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.graphs_completed OWNER TO gcross;

--
-- TOC entry 1491 (class 1259 OID 16492)
-- Dependencies: 1762 1763 3
-- Name: hamiltonians; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE hamiltonians (
    code_id uuid NOT NULL,
    edge integer NOT NULL,
    label1 integer NOT NULL,
    label2 integer NOT NULL,
    CONSTRAINT label1_is_pauli CHECK (((1 <= label1) AND (label1 <= 3))),
    CONSTRAINT label2_is_pauli CHECK (((1 <= label2) AND (label2 <= 3)))
);


ALTER TABLE public.hamiltonians OWNER TO gcross;

--
-- TOC entry 1488 (class 1259 OID 16430)
-- Dependencies: 3 1487
-- Name: logical_qubits; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE logical_qubits (
    mwe_operator_id uuid
)
INHERITS (encoded_qubits);


ALTER TABLE public.logical_qubits OWNER TO gcross;

--
-- TOC entry 1484 (class 1259 OID 16397)
-- Dependencies: 1760 3
-- Name: operators; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE operators (
    operator_id uuid NOT NULL,
    vertex integer NOT NULL,
    label integer NOT NULL,
    CONSTRAINT must_be_pauli CHECK (((1 <= label) AND (label <= 3)))
);


ALTER TABLE public.operators OWNER TO gcross;

--
-- TOC entry 1486 (class 1259 OID 16406)
-- Dependencies: 3
-- Name: qubits; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE qubits (
    qubit_id uuid NOT NULL,
    x_operator_id uuid NOT NULL,
    y_operator_id uuid NOT NULL,
    z_operator_id uuid NOT NULL
);


ALTER TABLE public.qubits OWNER TO gcross;

--
-- TOC entry 1485 (class 1259 OID 16401)
-- Dependencies: 3
-- Name: stabilizers; Type: TABLE; Schema: public; Owner: gcross; Tablespace: 
--

CREATE TABLE stabilizers (
    code_id uuid NOT NULL,
    operator_id uuid NOT NULL
);


ALTER TABLE public.stabilizers OWNER TO gcross;

--
-- TOC entry 1780 (class 2606 OID 16454)
-- Dependencies: 1490 1490
-- Name: codes_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY codes
    ADD CONSTRAINT codes_primary_key PRIMARY KEY (code_id);


--
-- TOC entry 1778 (class 2606 OID 16446)
-- Dependencies: 1489 1489 1489
-- Name: gauge_qubits_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY gauge_qubits
    ADD CONSTRAINT gauge_qubits_primary_key PRIMARY KEY (code_id, qubit_id);


--
-- TOC entry 1786 (class 2606 OID 24592)
-- Dependencies: 1493 1493
-- Name: graphs_being_scanned_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY graphs_being_scanned
    ADD CONSTRAINT graphs_being_scanned_primary_key PRIMARY KEY (graph);


--
-- TOC entry 1784 (class 2606 OID 24583)
-- Dependencies: 1492 1492
-- Name: graphs_completed_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY graphs_completed
    ADD CONSTRAINT graphs_completed_primary_key PRIMARY KEY (graph);


--
-- TOC entry 1782 (class 2606 OID 16498)
-- Dependencies: 1491 1491 1491
-- Name: hamiltonians_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY hamiltonians
    ADD CONSTRAINT hamiltonians_primary_key PRIMARY KEY (code_id, edge);


--
-- TOC entry 1775 (class 2606 OID 16434)
-- Dependencies: 1488 1488 1488
-- Name: logical_qubits_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY logical_qubits
    ADD CONSTRAINT logical_qubits_primary_key PRIMARY KEY (code_id, qubit_id);


--
-- TOC entry 1768 (class 2606 OID 16414)
-- Dependencies: 1484 1484 1484
-- Name: operator_and_vertex_is_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY operators
    ADD CONSTRAINT operator_and_vertex_is_primary_key PRIMARY KEY (operator_id, vertex);


--
-- TOC entry 1772 (class 2606 OID 16410)
-- Dependencies: 1486 1486
-- Name: qubit_id_is_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY qubits
    ADD CONSTRAINT qubit_id_is_primary_key PRIMARY KEY (qubit_id);


--
-- TOC entry 1770 (class 2606 OID 16441)
-- Dependencies: 1485 1485 1485
-- Name: stabilizers_primary_key; Type: CONSTRAINT; Schema: public; Owner: gcross; Tablespace: 
--

ALTER TABLE ONLY stabilizers
    ADD CONSTRAINT stabilizers_primary_key PRIMARY KEY (code_id, operator_id);


--
-- TOC entry 1773 (class 1259 OID 16476)
-- Dependencies: 1487
-- Name: fki_encoded_code_id; Type: INDEX; Schema: public; Owner: gcross; Tablespace: 
--

CREATE INDEX fki_encoded_code_id ON encoded_qubits USING btree (code_id);


--
-- TOC entry 1776 (class 1259 OID 16465)
-- Dependencies: 1489
-- Name: fki_gauge_qubit_id; Type: INDEX; Schema: public; Owner: gcross; Tablespace: 
--

CREATE INDEX fki_gauge_qubit_id ON gauge_qubits USING btree (qubit_id);


--
-- TOC entry 1788 (class 2606 OID 16471)
-- Dependencies: 1779 1490 1487
-- Name: encoded_code_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY encoded_qubits
    ADD CONSTRAINT encoded_code_id FOREIGN KEY (code_id) REFERENCES codes(code_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1791 (class 2606 OID 16455)
-- Dependencies: 1489 1490 1779
-- Name: gauge_code_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY gauge_qubits
    ADD CONSTRAINT gauge_code_id FOREIGN KEY (code_id) REFERENCES codes(code_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1792 (class 2606 OID 16466)
-- Dependencies: 1771 1489 1486
-- Name: gauge_qubit_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY gauge_qubits
    ADD CONSTRAINT gauge_qubit_id FOREIGN KEY (qubit_id) REFERENCES qubits(qubit_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1793 (class 2606 OID 16499)
-- Dependencies: 1490 1779 1491
-- Name: hamiltonian_code_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY hamiltonians
    ADD CONSTRAINT hamiltonian_code_id FOREIGN KEY (code_id) REFERENCES codes(code_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 1789 (class 2606 OID 16477)
-- Dependencies: 1490 1488 1779
-- Name: logical_code_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY logical_qubits
    ADD CONSTRAINT logical_code_id FOREIGN KEY (code_id) REFERENCES codes(code_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1790 (class 2606 OID 16482)
-- Dependencies: 1488 1486 1771
-- Name: logical_qubit_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY logical_qubits
    ADD CONSTRAINT logical_qubit_id FOREIGN KEY (qubit_id) REFERENCES qubits(qubit_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1787 (class 2606 OID 16487)
-- Dependencies: 1485 1779 1490
-- Name: stabilizers_code_id; Type: FK CONSTRAINT; Schema: public; Owner: gcross
--

ALTER TABLE ONLY stabilizers
    ADD CONSTRAINT stabilizers_code_id FOREIGN KEY (code_id) REFERENCES codes(code_id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 1797 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- TOC entry 1798 (class 0 OID 0)
-- Dependencies: 1490
-- Name: codes; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE codes FROM PUBLIC;
REVOKE ALL ON TABLE codes FROM gcross;
GRANT ALL ON TABLE codes TO gcross;
GRANT INSERT ON TABLE codes TO codescan;
GRANT SELECT ON TABLE codes TO PUBLIC;


--
-- TOC entry 1799 (class 0 OID 0)
-- Dependencies: 1487
-- Name: encoded_qubits; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE encoded_qubits FROM PUBLIC;
REVOKE ALL ON TABLE encoded_qubits FROM gcross;
GRANT ALL ON TABLE encoded_qubits TO gcross;
GRANT SELECT ON TABLE encoded_qubits TO PUBLIC;


--
-- TOC entry 1800 (class 0 OID 0)
-- Dependencies: 1489
-- Name: gauge_qubits; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE gauge_qubits FROM PUBLIC;
REVOKE ALL ON TABLE gauge_qubits FROM gcross;
GRANT ALL ON TABLE gauge_qubits TO gcross;
GRANT INSERT ON TABLE gauge_qubits TO codescan;
GRANT SELECT ON TABLE gauge_qubits TO PUBLIC;


--
-- TOC entry 1801 (class 0 OID 0)
-- Dependencies: 1493
-- Name: graphs_being_scanned; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE graphs_being_scanned FROM PUBLIC;
REVOKE ALL ON TABLE graphs_being_scanned FROM gcross;
GRANT ALL ON TABLE graphs_being_scanned TO gcross;
GRANT INSERT,DELETE,UPDATE ON TABLE graphs_being_scanned TO codescan;
GRANT SELECT ON TABLE graphs_being_scanned TO PUBLIC;


--
-- TOC entry 1802 (class 0 OID 0)
-- Dependencies: 1492
-- Name: graphs_completed; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE graphs_completed FROM PUBLIC;
REVOKE ALL ON TABLE graphs_completed FROM gcross;
GRANT ALL ON TABLE graphs_completed TO gcross;
GRANT INSERT ON TABLE graphs_completed TO codescan;
GRANT SELECT ON TABLE graphs_completed TO PUBLIC;


--
-- TOC entry 1803 (class 0 OID 0)
-- Dependencies: 1491
-- Name: hamiltonians; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE hamiltonians FROM PUBLIC;
REVOKE ALL ON TABLE hamiltonians FROM gcross;
GRANT ALL ON TABLE hamiltonians TO gcross;
GRANT INSERT ON TABLE hamiltonians TO codescan;
GRANT SELECT ON TABLE hamiltonians TO PUBLIC;


--
-- TOC entry 1804 (class 0 OID 0)
-- Dependencies: 1488
-- Name: logical_qubits; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE logical_qubits FROM PUBLIC;
REVOKE ALL ON TABLE logical_qubits FROM gcross;
GRANT ALL ON TABLE logical_qubits TO gcross;
GRANT INSERT ON TABLE logical_qubits TO codescan;
GRANT SELECT ON TABLE logical_qubits TO PUBLIC;


--
-- TOC entry 1805 (class 0 OID 0)
-- Dependencies: 1484
-- Name: operators; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE operators FROM PUBLIC;
REVOKE ALL ON TABLE operators FROM gcross;
GRANT ALL ON TABLE operators TO gcross;
GRANT INSERT ON TABLE operators TO codescan;
GRANT SELECT ON TABLE operators TO PUBLIC;


--
-- TOC entry 1806 (class 0 OID 0)
-- Dependencies: 1486
-- Name: qubits; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE qubits FROM PUBLIC;
REVOKE ALL ON TABLE qubits FROM gcross;
GRANT ALL ON TABLE qubits TO gcross;
GRANT INSERT ON TABLE qubits TO codescan;
GRANT SELECT ON TABLE qubits TO PUBLIC;


--
-- TOC entry 1807 (class 0 OID 0)
-- Dependencies: 1485
-- Name: stabilizers; Type: ACL; Schema: public; Owner: gcross
--

REVOKE ALL ON TABLE stabilizers FROM PUBLIC;
REVOKE ALL ON TABLE stabilizers FROM gcross;
GRANT ALL ON TABLE stabilizers TO gcross;
GRANT INSERT ON TABLE stabilizers TO codescan;
GRANT SELECT ON TABLE stabilizers TO PUBLIC;


-- Completed on 2009-01-01 13:56:43 EST

--
-- PostgreSQL database dump complete
--

