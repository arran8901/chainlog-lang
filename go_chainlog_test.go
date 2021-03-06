package chainloglang

import (
	_ "embed"
	"fmt"
	"io/fs"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"
)

//go:embed tests/testlib.pl
var testlib string

func TestQuery(t *testing.T) {
	runTestsForDir(t, "tests/query")
}

func TestMsg(t *testing.T) {
	runTestsForDir(t, "tests/msg")
}

func TestLib(t *testing.T) {
	runTestsForDir(t, "tests/lib")
}

func TestDynamicKB(t *testing.T) {
	const program = `
human(john).
human(sophie).
	`
	var dynamicKB = []string{"human(mark)", "human(catherine)"}

	i := newTestInterpreter()
	if err := i.ConsultWithDynamicKB(program, dynamicKB); err != nil {
		panic(err)
	}

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "mark"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); DynamicKBAsLogicProgram(dynamicKB) != "human(mark).\nhuman(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(mark), human(catherine)}, got %s", dynamicKB)
	}

	// Should be able to retract dynamic fact.
	i.Retract(`human(mark)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); DynamicKBAsLogicProgram(dynamicKB) != "human(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine)}, got %s", dynamicKB)
	}

	// Should not be able to retract static fact.
	i.Retract(`human(sophie)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); DynamicKBAsLogicProgram(dynamicKB) != "human(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine)}, got %s", dynamicKB)
	}

	// Should be able to assert dynamic fact.
	i.Assert(`human(kyle)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "kyle"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); DynamicKBAsLogicProgram(dynamicKB) != "human(catherine).\nhuman(kyle).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine), human(kyle)}, got %s", dynamicKB)
	}

	// Should be able to retract multiple dynamic facts without retracting static facts.
	i.Retract(`human(_)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); len(dynamicKB) != 0 {
		t.Fatalf("Expected empty dynamic KB, got %s", dynamicKB)
	}

	// Dynamic KB from one interpreter should be loadable in a fresh interpreter.
	i.Assert(`human(vincent)`)
	i.Assert(`human(george)`)
	newDynamicKB := i.GetDynamicKB()

	fresh := newTestInterpreter()
	fresh.ConsultWithDynamicKB(program, newDynamicKB)

	expectQueryDerivations(t, fresh, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "vincent"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "george"}},
		&Derivation{Successful: false},
	)
	freshDynamicKB := fresh.GetDynamicKB()
	equal := len(freshDynamicKB) == len(newDynamicKB)
	for i, dynamicFact := range freshDynamicKB {
		equal = equal && dynamicFact == newDynamicKB[i]
	}
	if !equal {
		t.Fatalf("Expected dynamic KB {human(vincent), human(george)}, got %s", dynamicKB)
	}
}

func TestParametricDisasterInsurance(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/parametric_disaster_insurance.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const policyholder Address = Address("chainlog103v2kw0xnhdfrnzukphheq9zlm67xg8ykjhfrc")
	const dataSource Address = Address("chainlog1rdhj3kcg9kr9y7pm489z579n9eqcsqyq0rjmwd")
	const insurer Address = Address("chainlog12t7x9c08wyuurnw7ayhjvv7ycev29tstudcxrp")

	// 1656028800 is Jun 24 00:00 GMT
	if err := i.prologInterpreter.Exec(`
dyn rainfall(13, 1654819200).  % Jun 10
dyn rainfall(27, 1654905600).  % Jun 11
dyn rainfall(81, 1654992000).  % Jun 12
dyn rainfall(81, 1655078400).  % Jun 13
dyn rainfall(81, 1655164800).  % Jun 14
dyn rainfall(81, 1655251200).  % Jun 15
dyn rainfall(81, 1655337600).  % Jun 16
dyn rainfall(81, 1655424000).  % Jun 17
dyn rainfall(81, 1655510400).  % Jun 18
dyn rainfall(81, 1655596800).  % Jun 19
dyn rainfall(81, 1655683200).  % Jun 20
dyn rainfall(81, 1655769600).  % Jun 21
dyn rainfall(56, 1655856000).  % Jun 22
dyn rainfall(46, 1655942400).  % Jun 23
dyn rainfall(23, 1656028800).  % Jun 24

% dyn seismic_intensity('5+', 1672534801).  % After expiration

dyn wind_speed(99, 1655164800).  % Jun 14
	`); err != nil {
		panic(err)
	}

	// Expect cyclone 80%, flood 100%.
	expectQueryDerivations(t, i, `eligible_for(D, P)`,
		&Derivation{Successful: true, Unifications: map[string]string{"D": "cyclone", "P": "80"}},
		&Derivation{Successful: true, Unifications: map[string]string{"D": "flood", "P": "100"}},
		&Derivation{Successful: false},
	)

	// Attempt payout from wrong address.
	_, err := i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  dataSource,
		Value:   0,
		Time:    1656288000,
		Balance: 1000,
	})
	if !(strings.Contains(err.Error(), "require_error") && strings.Contains(err.Error(), fmt.Sprintf("policyholder(%s)", dataSource))) {
		t.Fatalf("Expected require_error with policyholder/1 but got %s", err.Error())
	}

	// Make a cyclone payout (from correct address).
	actions, err := i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  policyholder,
		Value:   0,
		Time:    1656288000, // Jun 27
		Balance: 1000,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "claimed(cyclone,1656288000)" &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(policyholder) &&
		actions[1].(TransferAction).Value == 80) {
		t.Fatalf("Expected actions [assert(claimed(cyclone,1656288000)), transfer(%s,80)], got %s", policyholder, actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Expect flood 100% but no longer cyclone.
	expectQueryDerivations(t, i, `eligible_for(D, P)`,
		&Derivation{Successful: true, Unifications: map[string]string{"D": "flood", "P": "100"}},
		&Derivation{Successful: false},
	)

	// Attempt cyclone payout when not eligible.
	_, err = i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  policyholder,
		Value:   0,
		Time:    1656374400, // Jun 28
		Balance: 920,
	})
	if !(strings.Contains(err.Error(), "require_error") &&
		strings.Contains(err.Error(), ", eligible_for(cyclone")) {
		t.Fatalf("Expected require_error with eligible_for(cyclone, _) but got %s", err.Error())
	}

	// Dynamic KB should show cyclone has been claimed.
	dynamicKB := i.GetDynamicKB()
	if dynamicKB[len(dynamicKB)-1] != "claimed(cyclone,1656288000)" {
		t.Fatalf("Expected dynamic KB with claimed(cyclone, 1656288000) at the end, got %s", dynamicKB)
	}

	// Claim attempt after policy expiration should fail.
	_, err = i.Message(`claimPayout(flood)`, &MessageContext{
		Sender:  policyholder,
		Value:   0,
		Time:    1672621200, // Jan 2 2023
		Balance: 920,
	})
	if !(strings.Contains(err.Error(), "require_error") && strings.Contains(err.Error(), "policy_expiration(1672534800), <(1672621200, 1672534800")) {
		t.Fatalf("Expected require_error with time < policy_expiration, got: %s\n", err.Error())
	}

	// Terminate before policy expiration should fail.
	_, err = i.Message(`terminate`, &MessageContext{
		Sender:  insurer,
		Time:    1672448400, // Dec 31 2022
		Balance: 920,
	})
	if !(strings.Contains(err.Error(), "require_error") && strings.Contains(err.Error(), "policy_expiration(1672534800), >(1672448400, 1672534800")) {
		t.Fatalf("Expected require_error with time > policy_expration, got: %s\n", err.Error())
	}

	// Terminate after policy expiration should succeed.
	// Expect contract balance to be transferred to insurer.
	actions, err = i.Message(`terminate`, &MessageContext{
		Sender:  insurer,
		Time:    1672621200, // Jan 2 2023
		Balance: 920,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "transfer" &&
		actions[0].(TransferAction).ToAddress == string(insurer) &&
		actions[0].(TransferAction).Value == 920) {
		t.Fatalf("Expected actions [transfer(%s, 920)], got %s", insurer, actions)
	}
}

func TestShippingNoRefund(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/shipping.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const supplier Address = Address("chainlog1yyqgaseervflylmajgppcyjeuj7u7fxmnp9am0")
	const carrier Address = Address("chainlog1l3nacxn04j0sjd38s6qn3tx6agh2m23jg3yek9")
	const supplier_smart_sensor Address = Address("chainlog1av4684vuacgty6uxgrwfyql2wzfrkhjtjh9v7g")
	const carrier_smart_sensor Address = Address("chainlog1cfmjc45797tuvvqguzaapjeylpfhvzr2y6yjam")
	const consignee_smart_sensor Address = Address("chainlog1edc9s9kpgrssvxusp3kllkt7e03pmse7u6n8vv")
	const shippingCost uint64 = 10

	// Make supplier payment,??expect single assert supplier_paid.
	actions, err := i.Message(`supplierPayment`, &MessageContext{
		Sender:  supplier,
		Value:   200,
		Time:    1654819200, // Jun 10
		Balance: 0,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "supplier_paid(200,1654819200)") {
		t.Fatalf("Expected actions [assert(supplier_paid(200,1654819200))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Dispatch shipment ID 23, expect single assert dispatched.
	actions, err = i.Message(`shipmentDispatched(23)`, &MessageContext{
		Sender: supplier_smart_sensor,
		Time:   1654819200, // Jun 10
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "dispatched(23,1654819200)") {
		t.Fatalf("Expected actions [assert(dispatched(23,1654819200))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Add monitoring data with no violations, expect single assert monitoring_data.
	actions, err = i.Message(`monitoringData(23, 10, 89)`, &MessageContext{
		Sender: carrier_smart_sensor,
		Time:   1654905600, // Jun 11
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "monitoring_data(23,10,89,1654905600)") {
		t.Fatalf("Expected actions [assert(monitoring_data(23,10,89,1654905600))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Shipment should not be damaged.
	expectQueryDerivations(t, i, `damaged(23)`,
		&Derivation{Successful: false, Unifications: nil},
	)

	// On delivery, full cost should be transferred to carrier.
	actions, err = i.Message(`shipmentDelivered(23)`, &MessageContext{
		Sender: consignee_smart_sensor,
		Time:   1654992000, // Jun 12
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "transfer" &&
		actions[0].(TransferAction).ToAddress == string(supplier) &&
		actions[0].(TransferAction).Value == 0 &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(carrier) &&
		actions[1].(TransferAction).Value == shippingCost) {
		t.Fatalf("Expected actions [transfer(%s,0), transfer(%s,%d)], got: %s", supplier, carrier, shippingCost, actions)
	}
}

func TestShippingDamagedTotalRefund(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/shipping.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const supplier Address = Address("chainlog1yyqgaseervflylmajgppcyjeuj7u7fxmnp9am0")
	const carrier Address = Address("chainlog1l3nacxn04j0sjd38s6qn3tx6agh2m23jg3yek9")
	const supplier_smart_sensor Address = Address("chainlog1av4684vuacgty6uxgrwfyql2wzfrkhjtjh9v7g")
	const carrier_smart_sensor Address = Address("chainlog1cfmjc45797tuvvqguzaapjeylpfhvzr2y6yjam")
	const consignee_smart_sensor Address = Address("chainlog1edc9s9kpgrssvxusp3kllkt7e03pmse7u6n8vv")
	const shippingCost uint64 = 10

	// Make supplier payment.
	actions, _ := i.Message(`supplierPayment`, &MessageContext{
		Sender: supplier,
		Value:  100,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Dispatch shipment ID 92.
	actions, _ = i.Message(`shipmentDispatched(92)`, &MessageContext{
		Sender: supplier_smart_sensor,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Add monitoring data with temperature violation, expect single assert monitoring_data.
	actions, err := i.Message(`monitoringData(92, 13, 85)`, &MessageContext{
		Sender: carrier_smart_sensor,
		Time:   1654905600, // Jun 11
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "monitoring_data(92,13,85,1654905600)") {
		t.Fatalf("Expected actions [assert(monitoring_data(92,13,85,1654905600))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Shipment should be damaged.
	expectQueryDerivations(t, i, `damaged(92)`,
		&Derivation{Successful: true, Unifications: map[string]string{}},
		&Derivation{Successful: false, Unifications: nil},
	)

	// On delivery, full cost should be transferred to supplier.
	actions, err = i.Message(`shipmentDelivered(92)`, &MessageContext{
		Sender: consignee_smart_sensor,
		Time:   1654992000, // Jun 12
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "transfer" &&
		actions[0].(TransferAction).ToAddress == string(supplier) &&
		actions[0].(TransferAction).Value == shippingCost &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(carrier) &&
		actions[1].(TransferAction).Value == 0) {
		t.Fatalf("Expected actions [transfer(%s,%d), transfer(%s,0)], got: %s", supplier, shippingCost, carrier, actions)
	}
}

func TestShippingLateTotalRefund(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/shipping.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const supplier Address = Address("chainlog1yyqgaseervflylmajgppcyjeuj7u7fxmnp9am0")
	const carrier Address = Address("chainlog1l3nacxn04j0sjd38s6qn3tx6agh2m23jg3yek9")
	const supplier_smart_sensor Address = Address("chainlog1av4684vuacgty6uxgrwfyql2wzfrkhjtjh9v7g")
	const carrier_smart_sensor Address = Address("chainlog1cfmjc45797tuvvqguzaapjeylpfhvzr2y6yjam")
	const consignee_smart_sensor Address = Address("chainlog1edc9s9kpgrssvxusp3kllkt7e03pmse7u6n8vv")
	const shippingCost uint64 = 10

	// Make supplier payment.
	actions, _ := i.Message(`supplierPayment`, &MessageContext{
		Sender: supplier,
		Value:  100,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Dispatch shipment ID 2.
	actions, _ = i.Message(`shipmentDispatched(2)`, &MessageContext{
		Sender: supplier_smart_sensor,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Add monitoring data with no violations.
	actions, _ = i.Message(`monitoringData(2, 11, 86)`, &MessageContext{
		Sender: carrier_smart_sensor,
		Time:   1654905600, // Jun 11
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Shipment should not be damaged.
	expectQueryDerivations(t, i, `damaged(2)`,
		&Derivation{Successful: false, Unifications: nil},
	)

	// On delivery, full cost should be transferred to supplier.
	actions, err := i.Message(`shipmentDelivered(2)`, &MessageContext{
		Sender: consignee_smart_sensor,
		Time:   1655337600, // Jun 16
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "transfer" &&
		actions[0].(TransferAction).ToAddress == string(supplier) &&
		actions[0].(TransferAction).Value == shippingCost &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(carrier) &&
		actions[1].(TransferAction).Value == 0) {
		t.Fatalf("Expected actions [transfer(%s,%d), transfer(%s,0)], got: %s", supplier, shippingCost, carrier, actions)
	}
}

func TestShippingPartialRefund(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/shipping.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const supplier Address = Address("chainlog1yyqgaseervflylmajgppcyjeuj7u7fxmnp9am0")
	const carrier Address = Address("chainlog1l3nacxn04j0sjd38s6qn3tx6agh2m23jg3yek9")
	const supplier_smart_sensor Address = Address("chainlog1av4684vuacgty6uxgrwfyql2wzfrkhjtjh9v7g")
	const carrier_smart_sensor Address = Address("chainlog1cfmjc45797tuvvqguzaapjeylpfhvzr2y6yjam")
	const consignee_smart_sensor Address = Address("chainlog1edc9s9kpgrssvxusp3kllkt7e03pmse7u6n8vv")
	const shippingCost uint64 = 10

	// Make supplier payment.
	actions, _ := i.Message(`supplierPayment`, &MessageContext{
		Sender: supplier,
		Value:  100,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Dispatch shipment ID 19.
	actions, _ = i.Message(`shipmentDispatched(19)`, &MessageContext{
		Sender: supplier_smart_sensor,
		Time:   1654819200, // Jun 10
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Add monitoring data with no violations.
	actions, _ = i.Message(`monitoringData(19, 12, 87)`, &MessageContext{
		Sender: carrier_smart_sensor,
		Time:   1654905600, // Jun 11
	})
	i.Assert(actions[0].(AssertAction).Term)

	// Shipment should not be damaged.
	expectQueryDerivations(t, i, `damaged(19)`,
		&Derivation{Successful: false, Unifications: nil},
	)

	// On delivery, half cost should be transferred to supplier and carrier.
	actions, err := i.Message(`shipmentDelivered(19)`, &MessageContext{
		Sender: consignee_smart_sensor,
		Time:   1655121600, // Jun 13 12:00pm
	})
	if err != nil {
		t.Fatal(err)
	}
	halfCost := shippingCost / 2
	if !(len(actions) == 2 &&
		actions[0].Kind() == "transfer" &&
		actions[0].(TransferAction).ToAddress == string(supplier) &&
		actions[0].(TransferAction).Value == halfCost &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(carrier) &&
		actions[1].(TransferAction).Value == halfCost) {
		t.Fatalf("Expected actions [transfer(%s,%d), transfer(%s,%d)], got: %s", supplier, halfCost, carrier, halfCost, actions)
	}
}

func TestPharmacogenomicsData(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/pharmacogenomics_data.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	// Insert some data
	actions, err := i.Message("insert('HLA-B', 57, 'abacavir', 'Improved', true, false)", &MessageContext{})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "observation('HLA-B',57,abacavir,'Improved',true,false)") {
		t.Fatalf("Expected actions [assert(observation('HLA-B',57,abacavir,'Improved',true,false))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	actions, _ = i.Message(`insert('HLA-B', 49, 'abacavir', 'Improved', true, false)`, &MessageContext{})
	i.Assert(actions[0].(AssertAction).Term)
	actions, _ = i.Message(`insert('HLA-B', 57, 'abacavir', 'Deteriorated', true, true)`, &MessageContext{})
	i.Assert(actions[0].(AssertAction).Term)
	actions, _ = i.Message(`insert('test', 49, 'abacavir', 'Improved', true, false)`, &MessageContext{})
	i.Assert(actions[0].(AssertAction).Term)
	actions, _ = i.Message(`insert('HLA-B', 49, 'test', 'Improved', true, false)`, &MessageContext{})
	i.Assert(actions[0].(AssertAction).Term)
	actions, _ = i.Message(`insert('HLA-B', 57, 'abacavir', 'Unchanged', true, false)`, &MessageContext{})
	i.Assert(actions[0].(AssertAction).Term)

	expectQueryDerivations(t, i, `query('HLA-B', _, _, Observations)`,
		&Derivation{Successful: true, Unifications: map[string]string{
			"Observations": "[observation('HLA-B',57,abacavir,'Improved',true,false),observation('HLA-B',49,abacavir,'Improved',true,false),observation('HLA-B',57,abacavir,'Deteriorated',true,true),observation('HLA-B',49,test,'Improved',true,false),observation('HLA-B',57,abacavir,'Unchanged',true,false)]",
		}},
		&Derivation{Successful: false},
	)

	expectQueryDerivations(t, i, `query(_, 49, 'abacavir', Observations)`,
		&Derivation{Successful: true, Unifications: map[string]string{
			"Observations": "[observation('HLA-B',49,abacavir,'Improved',true,false),observation(test,49,abacavir,'Improved',true,false)]",
		}},
		&Derivation{Successful: false},
	)
}

func TestLicence(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/licence.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	// Use, publish and comment should be forbidden initially.
	expectQueryDerivations(t, i, `forbidden(use)`, &Derivation{Successful: true})
	expectQueryDerivations(t, i, `forbidden(publish)`, &Derivation{Successful: true})
	expectQueryDerivations(t, i, `forbidden(comment)`, &Derivation{Successful: true})
}

func TestCrowdfunding(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/crowdfunding.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	const beneficiary Address = Address("chainlog1n0tekhkd77ttvpac52jst8764l2qm952ghygvz")

	// past_deadline should be false before deadline.
	expectQueryDerivationsWithContext(t, i, `past_deadline`, &QueryContext{
		Time: 1655164800, // Jun 14
	}, &Derivation{Successful: false})
	// reached_goal should be false
	expectQueryDerivations(t, i, `reached_goal`, &Derivation{Successful: false})
	// total_raised should be 0.
	expectQueryDerivations(t, i, `total_raised(Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "0"}},
		&Derivation{Successful: false},
	)
	// state should be fundraising (only).
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time:    1655164800, // Jun 14
		Balance: 0,
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "fundraising"}},
		&Derivation{Successful: false},
	)

	// Contribute should assert funded/2.
	actions, err := i.Message(`contribute`, &MessageContext{
		Sender:  "funder1",
		Value:   20000,
		Time:    1655164800,
		Balance: 0,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 1 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "funded(funder1,20000)") {
		t.Fatalf("Expected actions [assert(funded(funder1,20000))], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// reached_goal should still be false.
	expectQueryDerivations(t, i, `reached_goal`, &Derivation{Successful: false})
	// total_raised should be 20000
	expectQueryDerivations(t, i, `total_raised(Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "20000"}},
		&Derivation{Successful: false},
	)
	// total_funded_by should be 20000 for funder1.
	expectQueryDerivations(t, i, `sum_all(_X, funded(funder1, _X), Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "20000"}},
		&Derivation{Successful: false},
	)

	// Contribute some more from different funders.
	actions, _ = i.Message(`contribute`, &MessageContext{
		Sender:  "funder2",
		Value:   50000,
		Time:    1655251200, // Jun 15
		Balance: 0,
	})
	i.Assert(actions[0].(AssertAction).Term)
	actions, _ = i.Message(`contribute`, &MessageContext{
		Sender:  "funder3",
		Value:   15000,
		Time:    1655251200, // Jun 15
		Balance: 0,
	})
	i.Assert(actions[0].(AssertAction).Term)

	// reached_goal should still be false.
	expectQueryDerivations(t, i, `reached_goal`, &Derivation{Successful: false})
	// total_raised should be 85000
	expectQueryDerivations(t, i, `total_raised(Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "85000"}},
		&Derivation{Successful: false},
	)
	// total_funded_by should be 20000 for funder1.
	expectQueryDerivations(t, i, `sum_all(_X, funded(funder1, _X), Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "20000"}},
		&Derivation{Successful: false},
	)
	// total_funded_by should be 50000 for funder2.
	expectQueryDerivations(t, i, `sum_all(_X, funded(funder2, _X), Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "50000"}},
		&Derivation{Successful: false},
	)
	// total_funded_by should be 15000 for funder3.
	expectQueryDerivations(t, i, `sum_all(_X, funded(funder3, _X), Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "15000"}},
		&Derivation{Successful: false},
	)
	// state should still be fundraising (only).
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time:    1655251200, // Jun 15
		Balance: 85000,
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "fundraising"}},
		&Derivation{Successful: false},
	)

	// If the deadline is reached at this point, state should become unsuccessful.
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time: 1656720000, // Jul 2
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "unsuccessful"}},
		&Derivation{Successful: false},
	)
	// It should not be possible to claimFunds.
	_, err = i.Message(`claimFunds`, &MessageContext{
		Sender:  beneficiary,
		Value:   0,
		Time:    1656720000, // Jul 2
		Balance: 85000,
	})
	if !(strings.Contains(err.Error(), "require_error") && strings.Contains(err.Error(), "state(successful)")) {
		t.Fatalf("Expected require_error with state(successful) but got %s", err.Error())
	}
	// It should be possible to refund.
	actions, err = i.Message(`refund`, &MessageContext{
		Sender:  "funder2",
		Value:   0,
		Time:    1656720000, // Jul 2
		Balance: 85000,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "retract" &&
		strings.HasPrefix(actions[0].(RetractAction).Term, "funded(funder2,") &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == "funder2" &&
		actions[1].(TransferAction).Value == 50000) {
		t.Fatalf("Expected actions [retract(funder2, _), transfer(funder2, 50000)], got %s", actions)
	}
	i.Retract(actions[0].(RetractAction).Term)

	// It should be possible for a funder to contribute again.
	actions, _ = i.Message(`contribute`, &MessageContext{
		Sender:  "funder1",
		Value:   70000,
		Time:    1655337600, // Jun 16
		Balance: 0,
	})
	i.Assert(actions[0].(AssertAction).Term)
	// reached_goal should be true.
	expectQueryDerivations(t, i, `reached_goal`, &Derivation{Successful: true}, &Derivation{Successful: false})
	// total_raised should be 105000.
	expectQueryDerivations(t, i, `total_raised(Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "105000"}},
		&Derivation{Successful: false},
	)
	// state should still be fundraising (only).
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time:    1655337600, // Jun 16
		Balance: 105000,
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "fundraising"}},
		&Derivation{Successful: false},
	)
	// total_funded_by should be 90000 for funder1.
	expectQueryDerivations(t, i, `sum_all(_X, funded(funder1, _X), Total)`,
		&Derivation{Successful: true, Unifications: map[string]string{"Total": "90000"}},
		&Derivation{Successful: false},
	)

	// If the deadline is reached at this point, state should become successful.
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time: 1656720000, // Jul 2
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "successful"}},
		&Derivation{Successful: false},
	)
	// It should not be possible to refund.
	_, err = i.Message(`refund`, &MessageContext{
		Sender:  "funder1",
		Time:    1656720000, // Jul 2
		Balance: 105000,
	})
	if !(strings.Contains(err.Error(), "require_error") && strings.Contains(err.Error(), "state(unsuccessful)")) {
		t.Fatalf("Expected require_error with state(unsuccessful) but got %s", err.Error())
	}
	// It should be posssible to claimFunds.
	actions, err = i.Message(`claimFunds`, &MessageContext{
		Sender:  beneficiary,
		Time:    1656720000, // Jul 2
		Balance: 105000,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "claimed" &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == string(beneficiary) &&
		actions[1].(TransferAction).Value == 105000) {
		t.Fatalf("Expected actions [assert(claimed), transfer(%s, 105000)], got %s", beneficiary, actions)
	}
	i.Assert(actions[0].(AssertAction).Term)
	// state should become closed.
	expectQueryDerivationsWithContext(t, i, `state(State)`, &QueryContext{
		Time: 1656720000, // Jul 2
	},
		&Derivation{Successful: true, Unifications: map[string]string{"State": "closed"}},
		&Derivation{Successful: false},
	)
}

func expectQueryDerivations(t *testing.T, i *Interpreter, query string, expectedDerivations ...*Derivation) {
	itr, err := i.Query(query)
	if err != nil {
		t.Fatal(err)
	}
	defer itr.Close()
	expectQueryDerivationsHelper(t, i, query, itr, expectedDerivations...)
}

func expectQueryDerivationsWithContext(t *testing.T, i *Interpreter, query string, queryCtx *QueryContext, expectedDerivations ...*Derivation) {
	itr, err := i.QueryWithContext(query, queryCtx)
	if err != nil {
		t.Fatal(err)
	}
	defer itr.Close()
	expectQueryDerivationsHelper(t, i, query, itr, expectedDerivations...)
}

func expectQueryDerivationsHelper(t *testing.T, i *Interpreter, query string, itr *QueryIterator, expectedDerivations ...*Derivation) {
	for i, expectedDerivation := range expectedDerivations {
		actualDerivation, err := itr.Next()
		if err != nil {
			t.Fatal(err)
		}

		equal := expectedDerivation.Successful == actualDerivation.Successful &&
			len(expectedDerivation.Unifications) == len(actualDerivation.Unifications)
		for variable, unifiedTerm := range expectedDerivation.Unifications {
			equal = equal && actualDerivation.Unifications[variable] == unifiedTerm
		}

		if !equal {
			t.Errorf("For query %s, derivation %d: expected %+v, actual %+v", query, i, expectedDerivation, actualDerivation)
		}
	}
}

func runTestsForDir(t *testing.T, dir string) {
	// Walk directory, collecting files with extension '.chl' as test files.
	var testFilepaths []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			t.Fatal(err)
		}
		if !d.IsDir() && filepath.Ext(path) == ".chl" {
			testFilepaths = append(testFilepaths, path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}

	// Run tests for each test file.
	for _, path := range testFilepaths {
		runTestsForFile(t, path)
	}
}

func runTestsForFile(t *testing.T, testFilepath string) {
	fileBytes, _ := ioutil.ReadFile(testFilepath)
	fileSource := string(fileBytes)

	// Find the test predicates.
	testPredicates, err := getTestPredicates(fileSource)
	if err != nil {
		t.Errorf("Failed to load test file: %s (%s)", testFilepath, err)
		return
	}

	// If there are no test predicates, we are done.
	if len(testPredicates) == 0 {
		return
	}

	// Otherwise, call each test predicate and report any test failures.
	i := newTestInterpreter()
	i.Consult(fileSource)
	for _, testPredicate := range testPredicates {
		sol := i.prologInterpreter.QuerySolution(fmt.Sprintf(`%s.`, testPredicate))
		if err = sol.Err(); err != nil {
			t.Errorf("In %s [%s]: %s", testFilepath, testPredicate, err)
		}
	}

	if err == nil {
		t.Logf("All tests passed for file: %s %v", testFilepath, testPredicates)
	}
}

func newTestInterpreter() (i *Interpreter) {
	i, err := NewInterpreterWithBuiltins(testlib)
	if err != nil {
		panic(err)
	}
	return
}

func getTestPredicates(program string) ([]string, error) {
	userDefinedPredicates, err := getUserDefinedPredicates(program)
	if err != nil {
		return nil, err
	}

	var pis []string
	for _, userDefinedPredicate := range userDefinedPredicates {
		// Test predicates are the 0-ary predicates whose names start with "test"
		if strings.HasPrefix(userDefinedPredicate.Predicate, "test") &&
			userDefinedPredicate.Arity == 0 {
			pis = append(pis, userDefinedPredicate.Predicate)
		}
	}
	return pis, nil
}
