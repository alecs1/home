using UnityEngine;
using System.Collections;

public class MouseManager : MonoBehaviour {
	public GameObject stone;

	Ray camToMouseRay;
	float maxDist = 1000f;

	void Start() {
	}

	void Update() {
		//Debug.Log(Time.frameCount);
		//mouse or touch screen
		if (Input.touches.Length > 0) {
			for (int i = 0; i < Input.touches.Length; i++) {
				Touch touch = Input.touches[i];
				camToMouseRay = Camera.main.ScreenPointToRay(touch.position);
			}
		}
		else {
			camToMouseRay = Camera.main.ScreenPointToRay(Input.mousePosition);
		}

		//if the ray intersects the table, show a stone
		RaycastHit tableHit;

		int tableMask = LayerMask.GetMask("Table");
		if (Physics.Raycast(camToMouseRay, out tableHit, maxDist, tableMask)) {
			Vector3 newPos = tableHit.point;
			newPos.y = 0;
			stone.transform.position = newPos;
		}
		//Debug.Log (tableHit.ToString());

		if (Input.GetButton("Fire1")) {
			stone = (GameObject)Instantiate(stone, stone.transform.position, stone.transform.rotation);
		}

	}
}
